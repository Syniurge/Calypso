// Contributed by Elie Morisse, same license DMD uses

#include "aggregate.h"
#include "attrib.h"
#include "declaration.h"
#include "enum.h"
#include "identifier.h"
#include "import.h"
#include "init.h"
#include "identifier.h"
#include "template.h"
#include "scope.h"
#include "statement.h"
#include "id.h"
#include "driver/cl_options.h"

#include "cpp/calypso.h"
#include "cpp/cppmodule.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppimport.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppexpression.h"
#include "cpp/cpptemplate.h"
#include "cpp/ddmdstructor.h"

#include <stdlib.h>
#include <string>

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/ModuleMap.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "clang/Sema/Lookup.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

/********************************/

File *setOutCalypsoFile(const char *path, const char *arg, const char *ext)
{
    const char *argobj;
    if (global.params.preservePaths)
        argobj = arg;
    else
        argobj = FileName::name(arg);

    if (!opts::cppCacheDir.empty())
        path = opts::cppCacheDir.c_str();

    assert(!FileName::absolute(argobj));
    argobj = FileName::combine(path, argobj);

    FileName::ensurePathExists(FileName::path(argobj));

    // always append the extension! otherwise hard to make output switches consistent
    return File::create(FileName::forceExt(argobj, ext));
}

Package *Module::rootPackage;
Modules Module::amodules;

void Module::init()
{
    rootPackage = new_Package(calypso.id_Scpp);
    rootPackage->symtab = new_DsymbolTable();

    modules->insert(rootPackage);
}

Module::Module(const char* filename, Identifier* ident, Identifiers *packages)
{
    // e.g __cpp_package_package_module
    llvm::SmallString<24> objFilename("__cpp-");
    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];
        objFilename += pid->toChars();
        objFilename += "-";
    }
    objFilename += ident->toChars();

    construct_Module(this, strdup(objFilename.c_str()), ident, 0, 0);

    loadEmittedSymbolList();

    // FIXME 1.1: needs rework to avoid dup code
    const char* objExt = nullptr;
    if (global.params.output_o)
        objExt = global.obj_ext;
    else if (global.params.output_bc)
        objExt = global.bc_ext;
    else if (global.params.output_ll)
        objExt = global.ll_ext;
    else if (global.params.output_s)
        objExt = global.s_ext;

    if (objExt)
        objfile = setOutCalypsoFile(/*global.params.objname, */global.params.objdir, arg, objExt);
}

Dsymbol *Module::search(Loc loc, Identifier *ident, int flags)
{
    auto result = ::Module::search(loc, ident, flags);

    if ((flags & IgnorePrivateImports) && result && result->isImport())
        return nullptr; // semi-HACK? this makes the imports inside an imported module invisible to D code,
                                // but also prevents conflicts caused by the choice to name C++ modules after record names.

    return result;
}

void Module::addPreambule()
{
    // Statically import object.d for object and size_t (used by buildXtoHash)
    // and cpp.core to initialize the foreign exception handler.

    // TODO This still makes "object" susceptible to collide with C++ names.
    // We could eventually choose a random unused alias name if necessary.
    if (members->dim == 0 || ((*members)[0])->ident != Id::object)
    {
        { // cpp.core
            auto packages = new Identifiers;
            packages->push(calypso.id_cpp);
            ::Import *im = new_Import(Loc(), packages, calypso.id_core, nullptr, true);
            members->shift(im);
        }
        // the C++ special import for global namespace lookups
        members->shift(new GlobalImport(Loc()));
        { // object
            ::Import *im = new_Import(Loc(), nullptr, Id::object, nullptr, true);
            members->shift(im);
        }
    }
}

inline char *strtok_rs(char *str, const char *delim, char **saveptr)
{
#if defined(_MSC_VER)
  return strtok_s(str, delim, saveptr);
#else
  return strtok_r(str, delim, saveptr);
#endif
}

void Module::loadEmittedSymbolList()
{
    auto symlistfile = setOutCalypsoFile(global.params.objdir, arg, "slist");
    if (symlistfile->read()) {
        delete_File(symlistfile);
        return;
    }

    char* stateptr;
    for (auto line = strtok_rs((char*) symlistfile->buffer, "\n", &stateptr); line;
                line = strtok_rs(nullptr, "\n", &stateptr))
        emittedSymbols.insert(std::string(line));

    delete_File(symlistfile);
}

void Module::saveEmittedSymbolList()
{
    if (emittedSymbols.empty())
        return;

    std::string buf;
    llvm::raw_string_ostream os(buf);

    for (auto& symname: emittedSymbols)
        os << symname << "\n";
    os.flush();
    buf.pop_back();

    auto symlistfile = setOutCalypsoFile(global.params.objdir, arg, "slist");
    symlistfile->setbuffer(const_cast<void*>((const void*) buf.data()), buf.size());
    if (symlistfile->write())
        ::error(Loc(), "Writing the symbol list file failed");
    symlistfile->setbuffer(nullptr, 0);
    delete_File(symlistfile);
}

/************************************/

// DeclMapper::DeclMapper(Module* mod)
//     : TypeMapper(mod)
// {
// }

inline PROTKIND DeclMapper::toProt(clang::AccessSpecifier AS)
{
    switch(AS) {
        case clang::AS_public:
            return PROTpublic;
        case clang::AS_protected:
            return PROTprotected;
        case clang::AS_private:
            return PROTprivate;
        case clang::AS_none:
            return PROTnone;
    }
}

/*****/

namespace
{

inline Dsymbols *oneSymbol(Dsymbol *s)
{
    auto decldefs = new Dsymbols;
    decldefs->push(s);
    return decldefs;
}

}

Dsymbols *DeclMapper::VisitDeclContext(const clang::DeclContext *DC)
{
    auto decldefs = new Dsymbols;

    for (auto D = DC->decls_begin(), DEnd = DC->decls_end();
        D != DEnd; ++D)
    {
        if (auto d = VisitDecl(*D))
            decldefs->append(d);
    }

    return decldefs;
}

Dsymbols *DeclMapper::VisitDecl(const clang::Decl *D, unsigned flags)
{
    if (D != getCanonicalDecl(D))
        return nullptr;

    // Sometimes the canonical decl of an explicit spec isn't the one in the parent DeclContext->decls
    // but the decl in FunctionTemplateDecl->specs, ex.: __convert_to_v in locale_facets.h
    // Which is why we map each spec in VisitRedeclarableTemplateDecl and need a flag to ensure that
    // they get mapped only once.
    if (!(flags & MapExplicitSpecs))
    {
        if (isa<clang::ClassTemplateSpecializationDecl>(D))
            return nullptr;

        if (auto FD = dyn_cast<clang::FunctionDecl>(D))
            if (FD->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization &&
                        FD->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization)
                return nullptr;
    }

    if (!(flags & MapTemplatePatterns))
    {
        if (auto Var = dyn_cast<clang::VarDecl>(D))
            if (Var->getDescribedVarTemplate())
                return nullptr;
    }

    Dsymbols *s = nullptr;

#define DECL(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D);
#define DECLWF(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D, flags);

    if (0) ;
    DECL(TypedefName)
    DECL(ClassTemplateSpecialization)
    DECL(VarTemplateSpecialization)
    DECLWF(Record)
    DECLWF(Function)
    DECL(RedeclarableTemplate)
    DECL(Enum)
    DECL(Value)

#undef DECL
#undef DECLWF

    return s;
}

Dsymbols *DeclMapper::VisitValueDecl(const clang::ValueDecl *D)
{
    auto& Context = calypso.getASTContext();
    ExprMapper expmap(*this);

    if (isa<clang::IndirectFieldDecl>(D)) // implicit fields injected from anon unions/structs, which are already mapped
        return nullptr;

    if (auto Field = dyn_cast<clang::FieldDecl>(D))
    {
        if (Field->isUnnamedBitfield())
            return nullptr;

        // NOTE: in union {...} myUnion isAnonymousStructOrUnion() will be false, it returns true only for "true" anonymous structs/unions
        if (Field->isAnonymousStructOrUnion())
        {
            auto a = VisitDecl(Field->getType()->castAs<clang::RecordType>()->getDecl(), MapAnonRecord);
            assert(a->dim == 1 && (*a)[0]->isAttribDeclaration());

            auto anon = static_cast<AnonDeclaration*>((*a)[0]->isAttribDeclaration());
            anon->AnonField = Field;
            return a;
        }
    }

    auto loc = fromLoc(D->getLocation());
    auto decldefs = new Dsymbols;

    auto II = D->getIdentifier();
    if (!II) {
        assert(D->isImplicit()); // lambda capture fields are unnamed and implicit
        return nullptr;
    }

    auto id = fromIdentifier(II);
    Type *t = nullptr;

    auto Ty = withoutNonAliasSugar(D->getType());
    if (auto RT = dyn_cast<clang::RecordType>(Ty.getTypePtr()))
    {
        auto RD = RT->getDecl();
        if (!RD->getIdentifier())
        {
            // Special case of union {...} myUnion; without D equivalment
            auto r = VisitRecordDecl(RD, NamedValueWithAnonRecord);
            if (!r)
                return nullptr;

            assert(r->dim == 1 && r->data[0]->isAggregateDeclaration());
            auto ad = static_cast<AggregateDeclaration*>(r->data[0]);

            t = ad->type;
            decldefs->push(ad);
        }
    }

    if (!t)
        t = fromType(D->getType(), loc);

    if (!t)
        return nullptr;

    if (t->isConst())
        t = t->immutableOf();

    auto a = new VarDeclaration(loc, id, D, t);

    if (auto Var = dyn_cast<clang::VarDecl>(D))
    {
        if (Var->hasExternalStorage())
            a->storage_class |= STCextern;

        if (Var->getTLSKind() == clang::VarDecl::TLS_Dynamic)
            a->storage_class |= STCtls;

        if (Var->isStaticDataMember())
            a->storage_class |= STCstatic;

        if ((Var->isConstexpr() || t->isImmutable()) &&
                Var->getAnyInitializer())
        {
            // we avoid initializer expressions except for const/constexpr variables
            auto Init = Var->getAnyInitializer();
            clang::APValue Eval;
            llvm::SmallVector<clang::PartialDiagnosticAt, 2> Diags;

            Expression *e = nullptr;
            if (!Init->isValueDependent() && Init->EvaluateAsInitializer(Eval, Context, Var, Diags))
                e = expmap.fromAPValue(loc, Eval, Var->getType()); // evaluating integer and boolean expressions is always preferable, because in some rare cases
                    // DMD and Clang's intepretations differ, one important instance being -1u < 0u (true for DMD, false for Clang)

            if (!e)
                e = expmap.fromExpression(Var->getAnyInitializer(), true);

            if (e && e->op != TOKnull)
                if (Init->isInstantiationDependent() || Init->isEvaluatable(Context))
                    a->_init = new_ExpInitializer(loc, e);
        }
    }

    decldefs->push(a);
    return decldefs;
}

// For simplicity's sake (or confusion's) let's call records with either virtual functions or bases polymorphic
bool isPolymorphic(const clang::RecordDecl *D)
{
    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();
    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
        return CRD && D->isCompleteDefinition() &&
            (CRD->getNumBases() || CRD->isPolymorphic());
}

Dsymbols *DeclMapper::VisitRecordDecl(const clang::RecordDecl *D, unsigned flags)
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.getSema();
    auto& Diags = *calypso.pch.Diags;
    auto Canon = D->getCanonicalDecl();

    if (D->isInjectedClassName())
        return nullptr;

    auto decldefs = new Dsymbols;
    auto loc = fromLoc(D->getLocation());

    if (S.RequireCompleteType(D->getLocation(),
                Context.getRecordType(D), clang::diag::err_incomplete_type))
        Diags.Reset();

    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();
    bool isDefined = D->isCompleteDefinition();
    bool isStruct = !isPolymorphic(D) && !(flags & ForcePolymorphic);

    auto TND = D->getTypedefNameForAnonDecl();

    int anon = 0;
    if (D->isAnonymousStructOrUnion())
    {
        assert(!TND);

        if (!(flags & MapAnonRecord))
          return nullptr;

        anon = 1;
        if (D->isUnion())
            anon = 2;
    }

    if (!anon && !getIdentifierOrNull(D) &&
                !(flags & NamedValueWithAnonRecord))
        return nullptr; // special case for union {} myUnion; which has no D equivalent

    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
        // NOTE: CXXRecordDecl will disappear in a future version of Clang and only
        // RecordDecl will remain to be used for both C and C++.

    auto members = new Dsymbols;

    AggregateDeclaration *a;
    if (!anon)
    {
        auto id = getIdentifierOrNull(D);

        if (D->isUnion())
        {
            a = new UnionDeclaration(loc, id, D);
        }
        else if (isStruct)
        {
            a = new StructDeclaration(loc, id, D);
        }
        else
        {
            auto baseclasses = new BaseClasses;

            if (CRD)
            {
                for (auto B = CRD->bases_begin(),
                        BEnd = CRD->bases_end(); B != BEnd; ++B)
                {
                    auto brt = fromType(B->getType(), loc);
                    baseclasses->push(new BaseClass(brt));
                }
            }

            auto cd = new ClassDeclaration(loc, id, baseclasses, members, CRD);
            a = cd;
        }
    }

    CXXScope.push(D);

    if (!isDefined)
        goto Ldeclaration;

    if (CRD)
    {
        if (!D->isUnion() && !CRD->isDependentType())
        {
            auto _CRD = const_cast<clang::CXXRecordDecl *>(CRD);

            // Clang declares and defines implicit ctors/assignment operators lazily,
            // but before D's semantic passes we at least need to declare them
            S.LookupDefaultConstructor(_CRD);
            for (int i = 0; i < 2; i++)
                S.LookupCopyingConstructor(_CRD, i ? clang::Qualifiers::Const : 0);

            S.LookupDestructor(_CRD);

            for (int i = 0; i < 2; i++)
                for (int j = 0; j < 2; j++)
                    for (int k = 0; k < 2; k++)
                        S.LookupCopyingAssignment(_CRD, i ? clang::Qualifiers::Const : 0, j ? true : false,
                                                k ? clang::Qualifiers::Const : 0);
        }
    }

    // Add specific decls: fields, vars, tags, templates, typedefs
    // They are expected by DMD to be in the correct order.
    for (auto M: D->decls())
    {
        if (cast<clang::Decl>(M->getDeclContext())->getCanonicalDecl() != Canon)
            continue;  /* only map declarations that are semantically within the RecordDecl */

        if (!isa<clang::FieldDecl>(M) && !isa<clang::VarDecl>(M) &&
              !isa<clang::FunctionDecl>(M) && !isa<clang::TagDecl>(M) &&
              !isa<clang::RedeclarableTemplateDecl>(M) && !isa<clang::TypedefNameDecl>(M))
            continue;

        if (auto s = VisitDecl(M))
            members->append(s);
    }

Ldeclaration:
    CXXScope.pop();

    if (anon)
        decldefs->push(new AnonDeclaration(loc, anon == 2, members));
    else
    {
        a->members = members;
        decldefs->push(a);
    }

    // Sometimes friend declarations are the only existing declarations, so map them to the parent context
    // see friend QString::operator==(const QString &s1, const QString &s2);
    // NOTE: should be after because ClassDeclaration::semantic() expects decldefs[0] to be the record
    typedef clang::DeclContext::specific_decl_iterator<clang::FriendDecl> Friend_iterator;
    auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(D);
    if (!CTSD || CTSD->getSpecializationKind() != clang::TSK_ImplicitInstantiation) // if we're in an implicit instantiation, no need to remap the out-of-line specializations which have their own template mapped with the class template
    {
        for (Friend_iterator I(D->decls_begin()), E(D->decls_end());
                    I != E; I++)
        {
            auto Decl = (*I)->getFriendDecl();
            if (!Decl || !Decl->isOutOfLine())
                continue;

            auto DeclCtx = dyn_cast<clang::DeclContext>(Decl);
            if (DeclCtx && DeclCtx->isDependentContext()) // FIXME: map them as template decls using the tpl from the record
                continue; // Later NOTE: dependent out-of-line free operators defined in a record as friend decl (ex. piecewise_linear_distribution::operator==) do not get added to the parent declcontext

            if (auto s = VisitDecl(Decl))
                decldefs->append(s);
        }
    }

    return decldefs;
}

Dsymbols *DeclMapper::VisitTypedefNameDecl(const clang::TypedefNameDecl* D)
{
    if (isAnonTagTypedef(D))
        return nullptr;  // the anon tag will be mapped by VisitRecordDecl to an aggregate named after the typedef identifier

    if (isSameNameTagTypedef(D)) // e.g typedef union pthread_attr_t pthread_attr_t needs to be discarded
        return nullptr;

    auto loc = fromLoc(D->getLocation());
    auto id = fromIdentifier(D->getIdentifier());
    auto t = fromType(D->getUnderlyingType(), loc);

    if (!t)
        return nullptr;

    auto a = new AliasDeclaration(loc, id, t, D);
    return oneSymbol(a);
}

TemplateParameters *initTempParams(Loc loc, SpecValue &spec)
{
    auto tpl = new TemplateParameters;
    TemplateParameter *p = nullptr;

    if (spec.op)
    {
        auto dstringty = new_TypeIdentifier(loc, Id::object);
        dstringty->addIdent(Id::string);

        auto tp_specvalue = new_StringExp(loc, const_cast<char*>(spec.op));
        p = new_TemplateValueParameter(loc, calypso.id_op,
                                            dstringty, tp_specvalue, nullptr);
    }
    else if (spec.t)
        p = new_TemplateTypeParameter(loc, calypso.id_type, spec.t, nullptr);

    if (p)
        tpl->push(p);
    return tpl;
}

bool isMapped(const clang::Decl *D)
{
    if (auto FD = dyn_cast<clang::FunctionDecl>(D))
    {
        if (D->isInvalidDecl())
            return false;

        if (isa<clang::FunctionNoProtoType>(FD->getType()))
            return false; // functions without prototypes are afaik builtins, and since D needs a prototype they can't be mapped

        auto Name = FD->getDeclName();
        if (Name.getNameKind() == clang::DeclarationName::CXXLiteralOperatorName)
            return false; // skip C++14 literal operators for now (always visible in MSVC 2015 regardless of compiler flags)

        if (Name.getNameKind() == clang::DeclarationName::CXXOperatorName)
            switch (Name.getCXXOverloadedOperator())
            {
                case clang::OO_New:
                case clang::OO_Delete:
                case clang::OO_Array_New:
                case clang::OO_Array_Delete:
                    return false; // skip new and delete operators for now (FIXME?)
                default:
                    break;
            }

        // Clang/MSVC's __builtin_addressof(void&) doesn't have a valid type
        // Do not map the builtin and eventually (TODO) handle it specifically
        if (FD->getBuiltinID() == clang::Builtin::BI__builtin_addressof)
            return false;

        if (auto MD = dyn_cast<clang::CXXMethodDecl>(D))
        {
            auto Parent = MD->getParent();
            if (Parent->isUnion())
                return false;
            if (MD->isTrivial()) {
                if (isa<clang::CXXDestructorDecl>(MD))
                    return false; // trivial dtors never get emitted by Clang
                else if (MD->isImplicit())
                    return false;
            }
        }

        // Clang runtime functions making use of target-specific intrisics always have __target__("feature") attributes
        // do not map the ones not supported by the target machine set by LDC
        if (auto TD = FD->getAttr<clang::TargetAttr>()) {
            auto ParsedAttr = TD->parse();
            for (auto& Feature: ParsedAttr.Features)
                if (!calypso.TargetFeatures.count(Feature))
                    return false;
        }
    }

    return true;
}

Dsymbols *DeclMapper::VisitFunctionDecl(const clang::FunctionDecl *D, unsigned flags)
{
    if (!isMapped(D))
        return nullptr;

    if (!(flags & MapTemplateInstantiations) && D->isTemplateInstantiation() &&
            D->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization)
        return nullptr;

    auto loc = fromLoc(D->getLocation());
    auto MD = dyn_cast<clang::CXXMethodDecl>(D);
    auto CCD = dyn_cast<clang::CXXConstructorDecl>(D);

    if (D->isInvalidDecl())
        return nullptr;

    auto FPT = D->getType()->castAs<clang::FunctionProtoType>();

    volatileNumber = 0; // reset the number of volatile qualifiers found
    auto tf = FromType(*this, loc).fromTypeFunction(FPT, D);
    if (!tf)
    {
        if (opts::cppVerboseDiags)
            ::warning(loc, "Discarding %s, non-supported argument or return type (e.g int128_t)",
                                D->getDeclName().getAsString().c_str());
        return nullptr;
    }
    assert(tf->ty == Tfunction);

    // If a function has overloads with the same signature except for volatile qualifiers, volatile overloads need to be
    // renamed to not interfere with the non-volatile ones (ex. std::atomic).
    bool prefixVolatile = false;
    auto funcVolatileNumber = volatileNumber;
    if (funcVolatileNumber)
    {
            auto R = getDeclContextNonLinkSpec(D)->lookup(D->getDeclName());

            for (auto Match: R)
            {
                if (auto MatchTemp = dyn_cast<clang::FunctionTemplateDecl>(Match))
                    Match = MatchTemp->getTemplatedDecl();

                if (Match->getCanonicalDecl() == D->getCanonicalDecl())
                    continue;

                auto Overload = dyn_cast<clang::FunctionDecl>(Match);
                if (!Overload || Overload->isTemplateInstantiation())
                    continue;
                auto OverloadType = Overload->getType()->castAs<clang::FunctionProtoType>();
                volatileNumber = 0;
                FromType(*this, loc).fromTypeFunction(OverloadType, Overload);

                if (volatileNumber < funcVolatileNumber) {
                    prefixVolatile = true;
                    break;
                }

                if (volatileNumber == funcVolatileNumber)
                    if (opts::cppVerboseDiags)
                        ::warning(loc, "Same number of volatile qualifiers found in another overload, things might break if they end up with the same D function type");
            }
    }

    StorageClass stc = STCundefined;
    if (MD)
    {
        if (MD->isDeleted())
            stc |= STCdisable;

        if (MD->isStatic())
            stc |= STCstatic;

        if (!MD->isVirtual())
            stc |= STCfinal;

        if (MD->isPure())
            stc |= STCabstract;

        if (MD->begin_overridden_methods()
                != MD->end_overridden_methods())
            stc |= STCoverride;

        if (isa<clang::CXXDestructorDecl>(D))
            stc &= ~(STCoverride|STCabstract); // dtors aren't added to D's VTBLs

        if (CCD && !CCD->isExplicit())
            stc |= STCimplicit;
    }
    tf->addSTC(stc);

    auto applyVolatilePrefix = [&] (Identifier *baseIdent) {
        if (!prefixVolatile)
            return baseIdent;

        std::string idStr(baseIdent->toChars(), baseIdent->length());
        // insert _vtlNUM_ backwards
        idStr.insert(0, "_");
        idStr.insert(0, std::to_string(funcVolatileNumber));
        idStr.insert(0, "_vtl");

        if (opts::cppVerboseDiags)
            ::warning(loc, "volatile overload %s renamed to %s", baseIdent->toChars(), idStr.c_str());
        return Identifier::idPool(idStr.c_str(), idStr.size());
    };
    
    auto a = new Dsymbols;
    ::FuncDeclaration *fd;
    if (auto CD = dyn_cast<clang::CXXConstructorDecl>(D))
    {
        fd = new CtorDeclaration(loc, stc, tf, CD);
    }
    else if (auto DD = dyn_cast<clang::CXXDestructorDecl>(D))
    {
        fd = new DtorDeclaration(loc, stc, Id::dtor, DD);
    }
    else if (D->isOverloadedOperator() || isa<clang::CXXConversionDecl>(D))
    {
        SpecValue spec(*this);
        auto opIdent = getIdentifierOrNull(D, &spec); // will return nullptr if the operator isn't supported by D
                            // TODO map the unsupported operators anyway

        if (!opIdent)
            return nullptr;

        // NOTE: C++ overloaded operators might be virtual, unlike D which are always final (being templates)
        //   Mapping the C++ operator to opBinary()() directly would make D lose info and overriding the C++ method impossible

        auto FuncTemp = D->getPrimaryTemplate();
        auto Name = FuncTemp ? FuncTemp->getDeclName() : D->getDeclName(); // if D is a template instantiation of a conversion operator,
            // then if the destination type depends on the the template parameters the decl name won't be found by the lookup inside the parent,
            // we need to take the primary template name. Ex.: operator Vec<float, 3>(); instantiated from template<_Tp, int n> operator Vec<_Tp, n>();

        auto R = getDeclContextNonLinkSpec(D)->lookup(Name);
        std::function<bool(const clang::NamedDecl*)> pred;
        SpecValue spec2(*this);
        if (isa<clang::TagDecl>(D->getDeclContext()))
            pred = [&](const clang::NamedDecl* _D)
                { return opIdent == getIdentifierOrNull(_D, &spec2); }; // member operator, simplest case
        else
        {
            // non member overloaded operators are trickier, since they end up in different modules and we need one alias per module
            auto OpTyDecl = isOverloadedOperatorWithTagOperand(D);
            if (!OpTyDecl)
                pred = [&](const clang::NamedDecl* _D)
                    { return opIdent == getIdentifierOrNull(_D, &spec2) && !isOverloadedOperatorWithTagOperand(_D); };
            else
                pred = [&](const clang::NamedDecl* _D)
                    { return opIdent == getIdentifierOrNull(_D, &spec2) && isOverloadedOperatorWithTagOperand(_D, OpTyDecl); };
        }

#if defined(_MSC_VER)
        // MSVC's find_if tries to assign a new value to First, whose operator= is disabled
        auto FirstOverload = R.begin();
        {
            auto _Last = R.end();
            for (; FirstOverload != _Last; ++FirstOverload)
                if (pred(*FirstOverload))
                    break;
        }
#else
        auto FirstOverload = std::find_if(R.begin(), R.end(), pred);
#endif
        assert(FirstOverload != R.end());
        bool isFirstOverloadInScope = (*FirstOverload)->getCanonicalDecl() == D->getCanonicalDecl();

        bool wrapInTemp = spec &&
                    !D->getDescribedFunctionTemplate() &&  // if it's a templated overloaded operator then the template declaration is already taken care of
                    !(D->isFunctionTemplateSpecialization() && D->isTemplateInstantiation());  // if we're instantiating a templated overloaded operator, we're after the function

        Identifier *fullIdent;
        if (wrapInTemp)
            fullIdent = getExtendedIdentifier(D, *this);
        else
            fullIdent = opIdent;

        auto funcIdent = applyVolatilePrefix(fullIdent);

        // Add the overridable method (or the static function)
        fd = new FuncDeclaration(loc, funcIdent, stc, tf, D);
        a->push(fd);

        if (wrapInTemp && isFirstOverloadInScope)
        {
            // Add the opUnary/opBinary/... template declaration aliasing fullIdent if none exists(important!)
            auto tpl = initTempParams(loc, spec);

            auto a_fwd = new_AliasDeclaration(loc, opIdent,
                                        new_TypeIdentifier(loc, fullIdent));

            // Enclose the forwarding function within the template declaration
            auto decldefs = new Dsymbols;
            decldefs->push(a_fwd);

            auto tempdecl = new_TemplateDeclaration(loc, opIdent, tpl, nullptr, decldefs);
            a->push(tempdecl);
        }

        return a;
    }
    else
    {
        auto id = fromIdentifier(D->getIdentifier());
        id = applyVolatilePrefix(id);
        fd = new FuncDeclaration(loc, id, stc, tf, D);
    }

    if (D->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization &&
            D->getPrimaryTemplate() && // forward-declared explicit specializations do not have their primary template set (stangely)
            !(flags & MapTemplateInstantiations))
    {
        auto tpl = new TemplateParameters;

        auto FT = D->getPrimaryTemplate();
        auto TPL = FT->getTemplateParameters();
        auto AI = D->getTemplateSpecializationArgs()->asArray().begin();

        TempParamListRAII TPLR(this, TPL);
        for (auto PI = TPL->begin(), PE = TPL->end();
            PI != PE; PI++)
        {
            auto tp = VisitTemplateParameter(*PI, AI);
            if (!tp)
                return nullptr;
            tpl->push(tp);

            if (AI) AI++;
        }

        auto decldefs = new Dsymbols;
        decldefs->push(fd);
        auto td = new TemplateDeclaration(loc, fd->ident, tpl, decldefs, D);
        return oneSymbol(td);
    }

    a->push(fd);
    return a;
}

bool isTemplateParameterPack(const clang::NamedDecl *Param)
{
    if (auto NTTPD = dyn_cast<clang::NonTypeTemplateParmDecl>(Param))
        return NTTPD->isParameterPack();
    else if (auto TTPD = dyn_cast<clang::TemplateTypeParmDecl>(Param))
        return TTPD->isParameterPack();
    else if (auto TempTemp = dyn_cast<clang::TemplateTemplateParmDecl>(Param))
        return TempTemp->isParameterPack();

    llvm::llvm_unreachable_internal();
}

Dsymbols *DeclMapper::VisitRedeclarableTemplateDecl(const clang::RedeclarableTemplateDecl *D)
{
    SpecValue spec(*this);

    auto loc = fromLoc(D->getLocation());
    auto id = getIdentifierOrNull(D, &spec);

    if (!id)
        return nullptr; // TODO: map unsupported overloaded operators

    auto CTD = dyn_cast<clang::ClassTemplateDecl>(D);
    auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D);
    auto VTD = dyn_cast<clang::VarTemplateDecl>(D);

    auto Def = getDefinition(D);

    auto tpl = initTempParams(loc, spec);
    auto TPL = Def->getTemplateParameters();

    auto decldefs = new Dsymbols;

    {
        TempParamListRAII TPLR(this, TPL);

        for (auto P : *TPL)
        {
            auto tp = VisitTemplateParameter(P);
            if (!tp)
                return nullptr; // should be extremely rare, e.g if there's a int128_t value parameter
            tpl->push(tp);
        }

        auto s = VisitDecl(getCanonicalDecl(Def->getTemplatedDecl()),
                    MapTemplatePatterns);

        if (!s)
            return nullptr;

        decldefs->append(s);

        if (FTD) {
            assert(s->dim && (*s)[0]->isFuncDeclaration());
            id = (*s)[0]->ident; // in case of volatile overloads the original ident may be prefixed
        }
    }

    auto td = new TemplateDeclaration(loc, id, tpl, decldefs, D);

    auto a = new Dsymbols;
    a->push(td);

    // Append explicit and partial specializations to the returned symbols as well
    if (CTD)
    {
        llvm::SmallVector<clang::ClassTemplatePartialSpecializationDecl *, 2> PS;
        const_cast<clang::ClassTemplateDecl*>(CTD)->getPartialSpecializations(PS);

        for (auto Spec: CTD->specializations())
            if (auto sp = VisitDecl(Spec->getCanonicalDecl(), MapExplicitSpecs))
                a->append(sp);

        for (auto PartialSpec: PS)
            if (auto sp = VisitDecl(PartialSpec->getCanonicalDecl(), MapExplicitSpecs))
                a->append(sp);
    }
    else if (FTD)
    {
        for (auto Spec: FTD->specializations())
            if (auto sp = VisitDecl(getCanonicalDecl(Spec), MapExplicitSpecs))
                a->append(sp);
    }
    else if (VTD)
    {
        llvm::SmallVector<clang::VarTemplatePartialSpecializationDecl *, 2> PS;
        const_cast<clang::VarTemplateDecl*>(VTD)->getPartialSpecializations(PS);

        for (auto Spec: VTD->specializations())
            if (auto sp = VisitDecl(getCanonicalDecl(Spec), MapExplicitSpecs))
                a->append(sp);

        for (auto PartialSpec: PS)
            if (auto sp = VisitDecl(PartialSpec->getCanonicalDecl(), MapExplicitSpecs))
                a->append(sp);
    }

    return a;
}

Identifier *DeclMapper::getIdentifierForTemplateNonTypeParm(const clang::NonTypeTemplateParmDecl *T)
{
    if (auto Id = T->getIdentifier())
        return fromIdentifier(Id);
    else
    {
        if (opts::cppVerboseDiags)
            ::warning(Loc(), "Generating identifier for anonymous C++ non-type template parameter");

        // This should only ever happen in template param decl mapping
        std::string str;
        llvm::raw_string_ostream OS(str);
        OS << "value_parameter_" << T->getDepth() << '_' << T->getIndex();

        return Identifier::idPool(OS.str().c_str(), OS.str().size());
    }
}

TemplateParameter *DeclMapper::VisitTemplateParameter(const clang::NamedDecl *Param,
                                                                      const clang::TemplateArgument *SpecArg)
{
    ExprMapper expmap(*this);
    TemplateParameter *tp;

    auto loc = fromLoc(Param->getLocation());
    Identifier *id;

    RootObject *specArg = nullptr;
    if (SpecArg)
    {
        auto a = FromType(*this, loc).fromTemplateArgument(SpecArg);
        if (!a->dim)
            return nullptr; // might a non-supported type
        specArg = (*a)[0];
    }

    if (auto NTTPD =
            dyn_cast<clang::NonTypeTemplateParmDecl>(Param))
    {
        id = getIdentifierForTemplateNonTypeParm(NTTPD);
        auto valTy = fromType(NTTPD->getType(), loc);

        if (!valTy)
            return nullptr;

        if (NTTPD->isParameterPack())
            return new_TemplateTupleParameter(loc, id);
        else
        {
            Expression *tp_specvalue = nullptr;
            Expression *tp_defaultvalue = nullptr;

            if (SpecArg)
            {
                switch (SpecArg->getKind())
                {
                    case clang::TemplateArgument::Expression:
                        tp_specvalue = expmap.fromExpression(SpecArg->getAsExpr());
                        break;
                    case clang::TemplateArgument::Integral:
                        tp_specvalue = expmap.fromAPInt(loc, SpecArg->getAsIntegral());
                        break;
                    case clang::TemplateArgument::NullPtr:
                        tp_specvalue = new_NullExp(Loc()/*, fromType(SpecArg->getNullPtrType())*/);
                        break;
                    default:
                        assert(false && "Unsupported template specialization value");
                }

                tp_specvalue = isExpression(specArg);
                if (!tp_specvalue)
                    return nullptr;
            }

            if (NTTPD->hasDefaultArgument())
            {
                tp_defaultvalue = expmap.fromExpression(NTTPD->getDefaultArgument());

//                 // TEMPORARY HACK: we choose a simple default value to make defaultArg()'s life easier
//                 //  (there were obscure identifier errors, e.g in __iterator_traits<_normal_iterator>).
//                 //  This doesn't affect anything other than reflection since the default argument evaluation is done by Clang
//                 //  We could btw use Clang's evaluation while keeping the mapped expression for reflection
//                 tp_defaultvalue = new_DotIdExp(loc, new_TypeExp(loc, valTy), Id::init);
            }

            tp = new_TemplateValueParameter(loc, id, valTy,
                                        tp_specvalue, tp_defaultvalue);
        }

    }
    else if (auto TTPD =
            dyn_cast<clang::TemplateTypeParmDecl>(Param))
    {
        id = getIdentifierForTemplateTypeParm(TTPD);

        if (TTPD->isParameterPack())
            return new_TemplateTupleParameter(loc, id);
        else
        {
            Type *tp_spectype = nullptr;
            Type *tp_defaulttype = nullptr;

            if (SpecArg)
            {
                tp_spectype = isType(specArg);
                if (!tp_spectype)
                    return nullptr;
            }

            if (TTPD->hasDefaultArgument())
                tp_defaulttype = fromType(TTPD->getDefaultArgument(), loc);

            tp = new_TemplateTypeParameter(loc, id, tp_spectype, tp_defaulttype);
        }
    }
    else if (auto TempTemp =
            dyn_cast<clang::TemplateTemplateParmDecl>(Param))
    {
        id = getIdentifierForTemplateTemplateParm(TempTemp);

        if (TempTemp->isParameterPack())
            return new_TemplateTupleParameter(loc, id);
        else
        {
            Type *tp_spectype = nullptr;
            Type *tp_defaulttype = nullptr;

            if (SpecArg)
            {
                tp_spectype = isType(specArg);
                if (!tp_spectype)
                    return nullptr;
            }

            if (TempTemp->hasDefaultArgument())
                tp_defaulttype = FromType(*this, loc).fromTemplateName(
                        TempTemp->getDefaultArgument().getArgument().getAsTemplate());

            tp = new_TemplateAliasParameter(loc, id, nullptr, tp_spectype, tp_defaulttype);
        }
    }
    else assert(false && "unrecognized template parameter");

    return tp;
}

Dsymbol *DeclMapper::VisitInstancedClassTemplate(const clang::ClassTemplateSpecializationDecl *D,
                                                 unsigned flags)
{
    assert(!isa<clang::ClassTemplatePartialSpecializationDecl>(D));

    rebuildScope(cast<clang::Decl>(D->getDeclContext()));

    std::unique_ptr<Dsymbols> a(
        VisitRecordDecl(D, flags | MapExplicitSpecs | MapTemplateInstantiations));
    assert(a->dim);
    return (*a)[0];
}

::FuncDeclaration *DeclMapper::VisitInstancedFunctionTemplate(const clang::FunctionDecl *D)
{
    rebuildScope(cast<clang::Decl>(D->getDeclContext()));

    std::unique_ptr<Dsymbols> a(VisitDecl(getCanonicalDecl(D), MapExplicitSpecs | MapTemplateInstantiations));
    if (a.get()) {
        assert(a->dim == 1 && (*a)[0]->isFuncDeclaration() && isCPP((*a)[0]));
        return static_cast<::FuncDeclaration*>((*a)[0]);
    }

    return nullptr;
}

::VarDeclaration * DeclMapper::VisitInstancedVarTemplate(const clang::VarTemplateSpecializationDecl * D)
{
    rebuildScope(cast<clang::Decl>(D->getDeclContext()));

    std::unique_ptr<Dsymbols> a(VisitDecl(getCanonicalDecl(D), MapExplicitSpecs | MapTemplateInstantiations));
    if (a.get()) {
        assert(a->dim == 1 && (*a)[0]->isVarDeclaration() && isCPP((*a)[0]));
        return static_cast<::VarDeclaration*>((*a)[0]);
    }

    return nullptr;
}

template<typename SpecTy>
inline Dsymbols* VisitSpecDecl(DeclMapper& declmap, const SpecTy* D) { assert(false); return nullptr; }

template<>
inline Dsymbols* VisitSpecDecl<clang::ClassTemplateSpecializationDecl>(
        DeclMapper& declmap, const clang::ClassTemplateSpecializationDecl* D) {
    return declmap.VisitRecordDecl(D);
}
template<>
inline Dsymbols* VisitSpecDecl<clang::VarTemplateSpecializationDecl>(
    DeclMapper& declmap, const clang::VarTemplateSpecializationDecl* D) {
    return declmap.VisitValueDecl(D);
}

// Partial and explicit specializations only
template<typename PartialTy, typename SpecTy>
Dsymbols* cpp::DeclMapper::VisitTemplateSpecializationDecl(const SpecTy* D)
{
    D = getDefinition(D);

    if (!D->isExplicitSpecialization())
        return nullptr;

    auto Partial = dyn_cast<PartialTy>(D);
    // NOTE: D's partial specializations != C++'s partial specializations
    // The mapping provides a "close" but not exact approximation of equivalent template specs in D (for reflection),
    // but TemplateDeclaration::findBestMatch is skipped since the choice is done by Clang anyway.

    auto loc = fromLoc(D->getLocation());
    auto id = fromIdentifier(D->getIdentifier());

    auto tpl = new TemplateParameters;

    auto Prim = getDefinition(D->getSpecializedTemplate());
    auto TPL = Prim->getTemplateParameters();
    auto AI = D->getTemplateArgs().asArray().begin();

    if (Partial) {
        TPL = Partial->getTemplateParameters(); // IMPORTANT NOTE: tiargs are made to match the partial spec params during TemplateInstance::semantic()
        AI = nullptr;
    }
    TempParamScope.push_back(TPL);
    CXXScope.push(D);

    for (auto PI = TPL->begin(), PE = TPL->end();
        PI != PE; PI++)
    {
        if (AI && isTemplateParameterPack(*PI)
                && AI->getKind() == clang::TemplateArgument::Pack
                && AI->pack_size() == 0)
            break; // ex.: std::tuple<> explicit spec of std::tuple<Elem..> // FIXME this doesn't work if there's an argument after a pack

        auto tp = VisitTemplateParameter(*PI, AI);
        if (!tp) {
            TempParamScope.pop_back();
            CXXScope.pop();
            return nullptr;
        }
        tpl->push(tp);

        if (AI) AI++;
    }

    if (Partial)
        // SEMI-HACK: even if we don't map partial specialization arguments, we still need to call fromTemplateArgument() to import module dependencies
        for (auto& SpecArg: D->getTemplateArgs().asArray())
            FromType(*this, loc).fromTemplateArgument(&SpecArg);

    CXXScope.pop();
    
    if (!Partial)
        TempParamScope.pop_back(); // the depth of template parameters does not consider explicit specs to be in the TempParamScope

    auto decldefs = VisitSpecDecl(*this, D);
    if (!decldefs)
        return nullptr;

    if (Partial)
        TempParamScope.pop_back();

    auto a = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    return oneSymbol(a);
}

Dsymbols* DeclMapper::VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D)
{
    return VisitTemplateSpecializationDecl<clang::ClassTemplatePartialSpecializationDecl>(D);
}

Dsymbols* DeclMapper::VisitVarTemplateSpecializationDecl(const clang::VarTemplateSpecializationDecl * D)
{
    return VisitTemplateSpecializationDecl<clang::VarTemplatePartialSpecializationDecl>(D);
}

Dsymbols *DeclMapper::VisitEnumDecl(const clang::EnumDecl* D)
{
    if (!D->isCompleteDefinition())
    {
        D = D->getDefinition();
        if (!D)
            return nullptr; // forward declaration without definition, skip
    }

    auto loc = fromLoc(D->getLocation());
    auto ident = getIdentifierOrNull(D);

    Type *memtype = nullptr;
    if (!D->isDependentType())
    {
        auto IntType = D->getIntegerType();
        if (IntType.isNull())
            IntType = D->getPromotionType();

        memtype = fromType(IntType, loc);
    }

    auto e = new EnumDeclaration(loc, ident, memtype, D);
    CXXScope.push(D);

    for (auto ECD: D->enumerators())
    {
        auto memberLoc = fromLoc(ECD->getLocation());

        if (!e->members)
            e->members = new Dsymbols;

        auto ident = fromIdentifier(ECD->getIdentifier());
        Expression *value = nullptr;

        if (auto InitE = ECD->getInitExpr())
        {
            value = ExprMapper(*this).fromExpression(InitE);
            value = new_CastExp(memberLoc, value, memtype); // SEMI-HACK (?) because the type returned by 1LU << ... will be ulong and we may need an int (see wctype.h)
        }

        auto em = new EnumMember(memberLoc, ident, value, nullptr, ECD);
        e->members->push(em);
    }

    CXXScope.pop();
    return oneSymbol(e);
}

Dsymbol* DeclMapper::VisitMacro(const clang::IdentifierInfo* II, const clang::Expr* E)
{
    auto TU = calypso.getASTContext().getTranslationUnitDecl();

    Loc loc;
    auto ident = fromIdentifier(II);

    // C allows macros and other symbols to share the same name, D doesn't
    // Conflicting macros get prefixed with ℂ
    if (!TU->lookup(clang::DeclarationName(II)).empty())
        ident = prefixConflictingIdentifier(ident);

    ExprMapper expmap(*this);
    auto e = expmap.fromExpression(E);
    auto ie = new_ExpInitializer(loc, e);

    auto v = new_VarDeclaration(loc, nullptr, ident, ie);
    v->storage_class = STCmanifest;
    return v;
}

/*****/

std::string moduleName(Identifiers *packages, Identifier *ident)
{
    std::string result = "__cpp/";
    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];
        result.append(pid->toChars(), pid->length());
        result.append("/");
    }
    result.append(ident->toChars(), ident->length());
    return result;
}

// Look into namespace redecls if there are any
static clang::DeclContext::lookup_result lookup(const clang::DeclContext *DC,
                                                   Identifier *id)
{
    auto& Table = calypso.getPreprocessor().getIdentifierTable();

    const char prefix[] = u8"ℂ";
    bool prefixed = strncmp(id->toChars(), prefix, sizeof(prefix)-1) == 0;
    auto& II = Table.get(!prefixed ? id->toChars() : id->toChars() + sizeof(prefix)-1);

    return DC->lookup(clang::DeclarationName(&II));
}

const clang::TagDecl *isOverloadedOperatorWithTagOperand(const clang::Decl *D,
                                const clang::NamedDecl *SpecificTag)
{
    auto& Context = calypso.getASTContext();

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
        Func = FuncTemp->getTemplatedDecl();

    if (!Func || !Func->isOverloadedOperator())
        return nullptr;

    if (Func->isFunctionTemplateSpecialization())
        Func = Func->getPrimaryTemplate()->getTemplatedDecl();

    assert(Func->getNumParams() > 0);
    if (Func->getNumParams() > 2)
        return nullptr; // [] and () cannot be non-member (FIXME: not entirely sure about (), couldn't find the source)

    if (auto ClassTemp = llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(SpecificTag))
        SpecificTag = ClassTemp->getTemplatedDecl();

    const clang::TagDecl *OpTyDecl = nullptr;

    for (unsigned I = 0; I < Func->getNumParams(); I++)
    {
        auto ParamTy = Func->getParamDecl(I)->getType().getNonReferenceType()
                                .getDesugaredType(Context).getCanonicalType();

        if (auto TempSpec = ParamTy->getAs<clang::TemplateSpecializationType>())
            if (auto Temp = TempSpec->getTemplateName().getAsTemplateDecl())
            {
                if (auto ClassTemp = dyn_cast<clang::ClassTemplateDecl>(Temp))
                {
                    OpTyDecl = ClassTemp->getTemplatedDecl();
                    break;
                }

                if (isa<clang::TypeAliasTemplateDecl>(Temp))
                    assert(false && "Wrongly assumed that it would get desugared");
            }

        if (auto TagTy = ParamTy->getAs<clang::TagType>())
        {
            OpTyDecl = TagTy->getDecl();
            break;
        }
    }

    if (OpTyDecl)
    {
        OpTyDecl = cast<clang::TagDecl>(GetNonNestedContext(OpTyDecl)); // get the module-level TagDecl

        if (auto TempSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(OpTyDecl))
            OpTyDecl = TempSpec->getSpecializedTemplate()->getTemplatedDecl();
    }

    if (OpTyDecl && // either LHS or RHS has a tag type
          (!SpecificTag || OpTyDecl->getCanonicalDecl() == SpecificTag->getCanonicalDecl())) // if we're looking for a specific type, compare it
        return OpTyDecl; 

    return nullptr;
}

static clang::Module *tryFindClangModule(Loc loc, Identifiers *packages, Identifier *id,
                                         Package *&p, size_t i)
{
#ifdef USE_CLANG_MODULES
    auto MMap = calypso.pch.MMap;

    if (!MMap)
        return nullptr;

    Package *pkg = p;
    clang::Module *M = nullptr;

    for (; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];

        llvm::StringRef name(pid->toChars(), pid->length());
        M = MMap->lookupModuleQualified(name, M);
        if (!M)
            return nullptr;

        pkg = static_cast<Package*>(pkg->symtab->lookup(pid));
        assert(pkg);
    }

    llvm::StringRef name(id->toChars(), id->length());
    M = MMap->lookupModuleQualified(name, M);
    if (!M)
        return nullptr;

    p = pkg;
    return M;
#else
    return nullptr;
#endif
}

static inline bool isTopLevelInNamespaceModule (const clang::Decl *D)
{
    auto Tag = dyn_cast<clang::TagDecl>(D);
    if (Tag && (Tag->getIdentifier() || Tag->getTypedefNameForAnonDecl()))
        return false; // anonymous tags are added as well

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    if (Func && Func->getDescribedFunctionTemplate())
        return false; // the function template will get mapped instead

    if (!Tag && !Func && !isa<clang::VarDecl>(D) &&
            !isa<clang::TypedefNameDecl>(D) &&
            !isa<clang::FunctionTemplateDecl>(D) &&
            !isa<clang::VarTemplateDecl>(D) &&
            !isa<clang::TypeAliasTemplateDecl>(D))
        return false;

    if (isOverloadedOperatorWithTagOperand(D))
        return false;  // non-member overloaded operators with class/struct/enum operands are included in their own module

    return true;
}

void mapMacros(DeclMapper &mapper, clang::Module::Header* Header, Dsymbols *members)
{
    auto MacroMapEntry = calypso.MacroMap[Header];
    if (!MacroMapEntry)
        return;

    for (auto& P: *MacroMapEntry)
        if (auto s = mapper.VisitMacro(P.first, P.second))
            members->push(s);   
}

static void mapNamespace(DeclMapper &mapper,
                             const clang::DeclContext *DC,
                             Dsymbols *members,
                             bool forClangModule = false)
{
    auto CanonDC = cast<clang::Decl>(DC)->getCanonicalDecl();
    auto MMap = calypso.pch.MMap;
    auto& SrcMgr = calypso.getSourceManager();

    auto D = DC->decls_begin(),
            DE = DC->decls_end();

    for (; D != DE; ++D)
    {
        if (cast<clang::Decl>(D->getDeclContext())->getCanonicalDecl()
                != CanonDC)
            continue;  // only map declarations that are semantically within the DeclContext

        auto DLoc = SrcMgr.getFileLoc((*D)->getLocation());
#ifdef USE_CLANG_MODULES
        if (!forClangModule && DLoc.isValid() && DLoc.isFileID()
                && MMap->findModuleForHeader(
                    SrcMgr.getFileEntryForID(SrcMgr.getFileID(DLoc))))
            continue;  // skip decls which are parts of a Clang module
#endif

        auto InnerNS = dyn_cast<clang::NamespaceDecl>(*D);
        if ((InnerNS && InnerNS->isInline()) || isa<clang::LinkageSpecDecl>(*D))
        {
            mapNamespace(mapper, cast<clang::DeclContext>(*D), members, forClangModule);
            continue;
        }
        else if (!isTopLevelInNamespaceModule(*D))
            continue;

        if (auto s = mapper.VisitDecl(*D))
            members->append(s);
    }
    
    if (DC->isTranslationUnit())
        mapMacros(mapper, nullptr, members);
}

static void mapClangModule(DeclMapper &mapper,
                             const clang::Decl *Root,
                             clang::Module *M,
                             Dsymbols *members)
{
    auto AST = calypso.getASTUnit();
    auto& SrcMgr = calypso.getSourceManager();

    llvm::SmallVector<clang::Decl*, 32> RegionDecls;

    // HACK-ish but Clang doesn't offer a straightforward way
    // SourceManager::translateFile() only offers the first FID of a FileEntry which doesn't contain all the decls,
    // so we need to loop over all the FID corresponding to Header.Entry.
    auto findRegionDecls = [&] (const clang::SrcMgr::SLocEntry& SLoc) {
        if (SLoc.isFile() && SLoc.getFile().getContentCache())
        {
            for (auto& Header: M->Headers[clang::Module::HK_Normal])
            {
                if (SLoc.getFile().getContentCache()->OrigEntry != Header.Entry)
                    continue;

                auto Loc = clang::SourceLocation::getFromRawEncoding(SLoc.getOffset());
                auto FID = SrcMgr.getFileID(Loc); // NOTE: getting a FileID without a SourceLocation is impossible, it's locked tight
                AST->findFileRegionDecls(FID, 0, SrcMgr.getFileIDSize(FID), RegionDecls); // passed Length is the maximum value before offset overflow kicks in
            }
        }
    };

    for (unsigned I = 0, N = SrcMgr.local_sloc_entry_size(); I != N; ++I)
        findRegionDecls(SrcMgr.getLocalSLocEntry(I));
    for (unsigned I = 0, N = SrcMgr.loaded_sloc_entry_size(); I != N; ++I)
        findRegionDecls(SrcMgr.getLoadedSLocEntry(I));

    // Not forgetting namespace redecls
    llvm::SmallVector<clang::Decl*, 8> RootDecls, ParentDecls;

    std::function<bool(const clang::Decl *)>
        Corresponding = [&] (const clang::Decl *D)
    {
        if (isa<clang::TranslationUnitDecl>(D))
            return true;

        if (!Corresponding(cast<clang::Decl>(getDeclContextNonLinkSpec(D))->getCanonicalDecl()))
            return false;

        ParentDecls.swap(RootDecls);
        RootDecls.clear();

        if (!ParentDecls.empty())
            for (auto Parent: ParentDecls)
            {
                auto DC = cast<clang::DeclContext>(Parent);
                for (auto ModDecl: DC->decls())
                    if (D->getCanonicalDecl() == ModDecl->getCanonicalDecl())
                        RootDecls.push_back(ModDecl);
            }
        else
            for (auto ModDecl: RegionDecls)
                if (isa<clang::TranslationUnitDecl>(getDeclContextNonLinkSpec(ModDecl)))
                    if (D->getCanonicalDecl() == ModDecl->getCanonicalDecl())
                        RootDecls.push_back(ModDecl);

        return !RootDecls.empty();
    };

    if (!Corresponding(Root))
    {
        ::error(Loc(), "Incorrect import, Clang module doesn't contain requested namespaces");
        fatal();
    }

    std::function<void(const clang::Decl *)> Map = [&] (const clang::Decl *D)
    {
        if (auto LinkSpec = dyn_cast<clang::LinkageSpecDecl>(D))
        {
            for (auto LD: LinkSpec->decls())
                Map(LD);
            return;
        }

        auto DC = D->getDeclContext();
        assert(isa<clang::TranslationUnitDecl>(DC)
                || isa<clang::NamespaceDecl>(DC)
                || isa<clang::LinkageSpecDecl>(DC));

        if (!isTopLevelInNamespaceModule(D))
            return;

        if (auto s = mapper.VisitDecl(D))
            members->append(s);
    };

    if (!isa<clang::TranslationUnitDecl>(Root))
        for (auto R: RootDecls)
            mapNamespace(mapper, cast<clang::DeclContext>(R), members, true);
    else
    {
        // Map the macros contained in the module headers (currently limited to numerical constants)
        for (auto& Header: M->Headers[clang::Module::HK_Normal])
            mapMacros(mapper, &Header, members);

        for (auto D: RegionDecls)
            if (isa<clang::TranslationUnitDecl>(D->getDeclContext()))
                Map(D);
    }
}

Module *Module::load(Loc loc, Identifiers *packages, Identifier *id, bool& isTypedef)
{
    if (!calypso.getASTUnit()) {
        ::error(loc, "Importing a C++ module without specifying C++ headers with pragma(cppmap, \"...\")");
        fatal();
    }

    auto& Context = calypso.getASTContext();
    auto& S = calypso.getSema();
    auto& Diags = calypso.pch.Diags;

    S.CurContext = Context.getTranslationUnitDecl();
    if (!S.TUScope)
        S.TUScope = new clang::Scope(nullptr, clang::Scope::DeclScope, *Diags);

    const clang::DeclContext *DC = Context.getTranslationUnitDecl();
    Package *pkg = rootPackage;

    assert(packages && packages->dim);

    clang::Module *M = nullptr;
    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];

        auto R = lookup(DC, pid);
        if (R.empty())
        {
            // Check if there's a Clang module matching the remaining packages.module.
            // Note that if there is a Clang module named QtCore, import Qt.QtCore is correct and
            // will import the declarations inside Qt:: and inside the headers listed in QtCore.
            M = tryFindClangModule(loc, packages, id, pkg, i);
            if (M)
                break;

            ::error(loc, "no C++ package named %s", pid->toChars());
            fatal();
        }

        auto NSN = dyn_cast<clang::NamespaceDecl>(R[0]);
        if (!NSN || NSN->isInline())
        {
            ::error(loc, "only non-inline namespaces can be C++ packages");
            fatal();
        }

        pkg = static_cast<Package*>(pkg->symtab->lookup(pid));
        assert(pkg);

        DC = NSN;
    }

    if (!M)
        M = tryFindClangModule(loc, packages, id, pkg, packages->dim);

    auto m = new Module(moduleName(packages, id).c_str(),
                        id, packages);
    m->members = new Dsymbols;
    m->parent = pkg;
    m->loc = loc;

    isTypedef = false;

    DeclMapper mapper(m);

    if (M)
    {
        auto D = cast<clang::Decl>(DC)->getCanonicalDecl();
        m->rootKey.first = D;
        m->rootKey.second = M;
        mapClangModule(mapper, D, M, m->members);
    }
    else if (id == calypso.id__)  // Hardcoded module with all the top-level non-tag decls + the anonymous tags of a namespace which aren't in a Clang module
    {
        m->rootKey.first = cast<clang::Decl>(DC)->getCanonicalDecl();

        auto NS = dyn_cast<clang::NamespaceDecl>(DC);
        if (!NS)
        {
            assert(isa<clang::TranslationUnitDecl>(DC));

            mapNamespace(mapper, DC, m->members);
        }
        else
        {
            auto I = NS->redecls_begin(),
                    E = NS->redecls_end();

            for (; I != E; ++I)
            {
                DC = *I;
                mapNamespace(mapper, DC, m->members);
            }
        }
    }
    else
    {
        clang::NamedDecl *D = nullptr;

        // Lookups can't find the implicit __va_list_tag record
        if (packages->dim == 1)
        {
            if (id == calypso.id___va_list_tag)
                D = cast<clang::NamedDecl>(Context.getVaListTagDecl());
            else if (id == calypso.id___NSConstantString_tag)
                D = Context.getCFConstantStringTagDecl(); // FIXME: this isn't satisfying, problem #1: not future-proof, problem #2: platform-dependent
                                                    // But this should be fixed if C++ import lookups get skipped (Calypso does A LOT of unnecessary name lookups
                                                    // and this was to stick as close as possible to DMD, but they're very expensive to build and do and skipping them
                                                    // would cut down multiple times compilation times)
        }

        if (!D)
        {
            auto R = lookup(DC, id);
            if (R.empty())
            {
                ::error(loc, "no C++ module named %s", id->toChars());
                fatal();
            }

            // Module must be a record or enum
            for (auto Match: R)
            {
                if (auto Typedef = dyn_cast<clang::TypedefNameDecl>(Match))
                {
                    if (auto Tag = isAnonTagTypedef(Typedef))
                        Match = const_cast<clang::TagDecl*>(Tag);
                    else if (!isSameNameTagTypedef(Typedef))
                    {
                        isTypedef = true;
                        delete m;
                        return nullptr; // a new attempt will be made by cpp::Import::loadModule after fixing its id
                    }
                }

                if (isa<clang::TagDecl>(Match) || isa<clang::ClassTemplateDecl>(Match))
                {
                    D = Match;
                    break;
                }
            }
        }

        if (!D)
        {
            ::error(loc, "C++ modules have to be enums, records (class/struct, template or not), typedefs or _.\n"
                            "Typedefs are included in _, and importing a typedef is equivalent to \"import (C++) _ : <typedef>;\"");
            fatal();
        }

        if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
            D = Spec->getSpecializedTemplate();

        D = cast<clang::NamedDecl>(D->getCanonicalDecl());
        auto CTD = dyn_cast<clang::ClassTemplateDecl>(D);

        if (CTD)
            m->rootKey.first = CTD->getTemplatedDecl();
        else
            m->rootKey.first = D;

        if (auto s = mapper.VisitDecl(D))
            m->members->append(s);

        // Add the non-member overloaded operators that are meant to work with this record/enum
        for (int Op = 1; Op < clang::NUM_OVERLOADED_OPERATORS; Op++)
        {
            auto OpName = Context.DeclarationNames.getCXXOperatorName(
                        static_cast<clang::OverloadedOperatorKind>(Op));

            for (auto Ctx = D->getDeclContext(); Ctx; Ctx = Ctx->getLookupParent())
            {
                if (Ctx->isTransparentContext())
                    continue;

                for (auto OverOp: Ctx->lookup(OpName))
                {
                    if (!isOverloadedOperatorWithTagOperand(OverOp, D))
                        continue;

                    if (OverOp->getFriendObjectKind() != clang::Decl::FOK_None && OverOp->isOutOfLine())
                        continue; // friend out-of-line decls are already mapped in VisitRecordDecl

                    if (auto s = mapper.VisitDecl(getCanonicalDecl(OverOp)))
                        m->members->append(s);
                }
            }
        }

//         srcFilename = AST->getSourceManager().getFilename(TD->getLocation());
    }

    // Enclose every mapped symbol into an extern(C++) declaration
    auto s = new_LinkDeclaration(LINKcpp, m->members);
    m->members = new Dsymbols;
    m->members->push(s);
    
    amodules.push_back(m);
    pkg->symtab->insert(m);
    return m;
}

}
