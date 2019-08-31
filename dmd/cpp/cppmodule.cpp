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
std::map<const clang::Decl*, Module*> Module::allCppModules;

void Module::init()
{
    rootPackage = new_Package(calypso.id_Scpp);
    rootPackage->symtab = new_DsymbolTable();

    modules->insert(rootPackage);
}

Module::Module(const char* filename, Identifier* ident)
{
    construct_Module(this, filename, ident, 0, 0);
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

inline Prot::Kind toProt(clang::AccessSpecifier AS)
{
    switch(AS) {
        case clang::AS_public:
            return Prot::public_;
        case clang::AS_protected:
            return Prot::protected_;
        case clang::AS_private:
            return Prot::private_;
        case clang::AS_none:
            return Prot::none;
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

inline void setDsym(const clang::NamedDecl* D, Dsymbol* sym)
{
    if (!D->d)
        const_cast<clang::NamedDecl*>(D)->d = new DData;
    assert(D->d->sym == nullptr);
    const_cast<clang::NamedDecl*>(D)->d->sym = sym;
}

}

bool isExplicitSpecialization(const clang::Decl *D)
{
    if (auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
        return ClassSpec->isExplicitSpecialization();
    else if (auto VarSpec = dyn_cast<clang::VarTemplateSpecializationDecl>(D))
        return VarSpec->isExplicitSpecialization();
    else if (auto Func = dyn_cast<clang::FunctionDecl>(D))
        return Func->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization &&
               Func->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization;
    else
        return false;
}

// IMPORTANT NOTE: this includes undefined instantiation declarations, e.g from typedefs
// For example in "typedef basic_fstream<char> fstream;", if fstream is unused, then
// basic_fstream<char> never gets defined
bool isTemplateInstantiation(const clang::Decl *D)
{
    if (auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
        return ClassSpec->getSpecializationKind() != clang::TSK_ExplicitSpecialization;
    else if (auto VarSpec = dyn_cast<clang::VarTemplateSpecializationDecl>(D))
        return VarSpec->getSpecializationKind() != clang::TSK_ExplicitSpecialization;
    else if (auto Func = dyn_cast<clang::FunctionDecl>(D))
        return Func->isTemplateInstantiation();
    return false;
}

Dsymbols *DeclMapper::VisitDecl(const clang::Decl *D, unsigned flags)
{
    if (D != getCanonicalDecl(D))
        return nullptr;

    auto ND = dyn_cast<clang::NamedDecl>(D);
    if (ND && ND->d && ND->d->mapped_syms)
        return ND->d->mapped_syms;

    Dsymbols *s = nullptr;

#define DECL(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D);
#define DECLWF(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D, flags);
#define DECLEXPLICIT(BASE) \
    else if ((flags & MapExplicitSpecs) && isa<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(cast<clang::BASE##Decl>(D));

    if (0) ;
    DECL(TypedefName)
    DECLEXPLICIT(ClassTemplateSpecialization)
    DECLEXPLICIT(VarTemplateSpecialization)
    DECL(Enum)
    DECLWF(Record)
    DECLWF(Function)
    DECLWF(Value)
    DECL(RedeclarableTemplate)

#undef DECL
#undef DECLWF

    bool mappedInnerSpec = !(flags & MapExplicitSpecs) && isExplicitSpecialization(D);

    // We don't want to attach the "inner" decl of template insts/specs to the module, it needs
    // to be the TemplateInstance, that we create if requested
    if (!mappedInnerSpec && (!isTemplateInstantiation(D) || (flags & CreateTemplateInstance)))
        if (ND && ND->d && s)
            const_cast<clang::NamedDecl*>(ND)->d->mapped_syms = s;

    return s;
}

template<typename SpecTy>
Dsymbols* DeclMapper::CreateTemplateInstanceFor(const SpecTy* D, Dsymbols* decldefs)
{
    auto TempDecl = D->getSpecializedTemplate();
    auto TempArgs = &D->getTemplateArgs();

    auto tempdecl = dsymForDecl(TempDecl);
    assert(tempdecl && tempdecl->isTemplateDeclaration());

    auto loc = (*decldefs)[0]->loc;
    auto tiargs = fromTemplateArguments<false>(loc, TempArgs, TempDecl->getTemplateParameters());
    auto ti = new TemplateInstance(loc, static_cast<TemplateDeclaration*>(tempdecl), tiargs);
    ti->members = decldefs;
    ti->isForeignInst = true;
    ti->inst = ti;
    ti->Inst = const_cast<SpecTy*>(D);

    assert(!static_cast<TemplateDeclaration*>(tempdecl)->findExistingInstance(ti, nullptr));
    static_cast<TemplateDeclaration*>(tempdecl)->addInstance(ti);

    decldefs = new Dsymbols;
    decldefs->push(ti);

    ti->minst = minst;
    ti->appendToModuleMember();

    return decldefs;
}

template<>
Dsymbols* DeclMapper::CreateTemplateInstanceFor<clang::FunctionDecl>(const clang::FunctionDecl* D, Dsymbols* decldefs)
{
    auto TempDecl = D->getPrimaryTemplate();
    auto TempArgs = D->getTemplateSpecializationArgs();

    auto tempdecl = dsymForDecl(TempDecl);
    assert(tempdecl && tempdecl->isTemplateDeclaration());
    auto tiargs = fromTemplateArguments<false>(TempArgs, TempDecl->getTemplateParameters());

    auto loc = (*decldefs)[0]->loc;
    auto ti = new TemplateInstance(loc, static_cast<TemplateDeclaration*>(tempdecl), tiargs);
    ti->members = decldefs;
    ti->isForeignInst = true;
    ti->inst = ti;
    ti->Inst = const_cast<clang::FunctionDecl*>(D);

    assert(!static_cast<TemplateDeclaration*>(tempdecl)->findExistingInstance(ti, nullptr));
    static_cast<TemplateDeclaration*>(tempdecl)->addInstance(ti);

    decldefs = new Dsymbols;
    decldefs->push(ti);

    ti->minst = minst;
    ti->appendToModuleMember();

    return decldefs;
}

Dsymbols *DeclMapper::VisitValueDecl(const clang::ValueDecl *D, unsigned flags)
{
    auto& Context = calypso.getASTContext();
    ExprMapper expmap(*this);

    if (auto Indirect = dyn_cast<clang::IndirectFieldDecl>(D))
        D = Indirect->getVarDecl();

    if (!(flags & MapTemplatePatterns))
        if (auto Var = dyn_cast<clang::VarDecl>(D))
            if (Var->getDescribedVarTemplate())
                return nullptr;

    if (auto Field = dyn_cast<clang::FieldDecl>(D))
    {
        if (Field->isUnnamedBitfield())
            return nullptr;

        // NOTE:  isAnonymousStructOrUnion() returns true only for "true" anonymous structs/unions
        //  for union {...} myUnion it will be false.
        if (Field->isAnonymousStructOrUnion())
        {
//             auto a = VisitDecl(Field->getType()->castAs<clang::RecordType>()->getDecl(), MapAnonRecord);
//             assert(a->dim == 1 && (*a)[0]->isAttribDeclaration());
//
//             auto anon = static_cast<AnonDeclaration*>((*a)[0]->isAttribDeclaration());
//             anon->AnonField = Field;
//             return a;
            return nullptr;
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
    setDsym(D, a);

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

    auto VarSpec = dyn_cast<clang::VarTemplateSpecializationDecl>(D);
    if (VarSpec && !VarSpec->isExplicitSpecialization() && (flags & CreateTemplateInstance))
        decldefs = CreateTemplateInstanceFor(VarSpec, decldefs);

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
    auto CanonDecl = D;
    auto& S = calypso.getSema();

    if (!isMapped(D))
        return nullptr;

//     if (D->isInjectedClassName())
//         return nullptr;
    assert(!D->isInjectedClassName());

    auto decldefs = new Dsymbols;
    auto loc = fromLoc(D->getLocation());

    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();

    int anon = 0;
    if (D->isAnonymousStructOrUnion())
    {
        assert(!D->getTypedefNameForAnonDecl());

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
    auto members = new Dsymbols;

    AggregateDeclaration *a;
    if (!anon)
    {
        auto id = getIdentifierOrNull(D);

        if (D->isUnion())
        {
            a = new UnionDeclaration(loc, id, D);
        }
        else if (isPolymorphic(D))
        {
            a = new StructDeclaration(loc, id, D);
        }
        else
        {
            a = new ClassDeclaration(loc, id, /*baseclasses =*/ nullptr, members, CRD);
        }

        setDsym(CanonDecl, a);

        if (auto cd = a->isClassDeclaration())
        {
            // Base classes must be mapped after attaching the Dsymbol to the Clang declaration,
            // in case the derived class gets referenced
            if (CRD)
            {
                for (auto B = CRD->bases_begin(),
                        BEnd = CRD->bases_end(); B != BEnd; ++B)
                {
                    auto brt = fromType(B->getType(), loc);
                    cd->baseclasses->push(new BaseClass(brt));

                    if (!cd->baseClass)
                        cd->baseClass = getAggregateSym(brt);
                }
            }
        }
    }

    if (anon)
        decldefs->push(new AnonDeclaration(loc, anon == 2, members));
    else
    {
        a->members = members;
        decldefs->push(a);

        auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D);
        if (ClassSpec && !ClassSpec->isExplicitSpecialization() && (flags & CreateTemplateInstance))
            decldefs = CreateTemplateInstanceFor(ClassSpec, decldefs);
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
    setDsym(D, a);
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

    if (auto RD = dyn_cast<clang::CXXRecordDecl>(D))
        if (RD->isLambda() && RD->getLambdaManglingNumber() == 0)
            return false;

    return true;
}

Dsymbols *DeclMapper::VisitFunctionDecl(const clang::FunctionDecl *D, unsigned flags)
{
    if (!isMapped(D))
        return nullptr;

    // Sometimes the canonical decl of an explicit spec isn't the one in the parent DeclContext->decls
    // but the decl in FunctionTemplateDecl->specs, ex.: __convert_to_v in locale_facets.h
    // Which is why we map each spec in VisitRedeclarableTemplateDecl and need a flag to ensure that
    // they get mapped only once.
    if (!(flags & MapExplicitSpecs) && isExplicitSpecialization(D))
        return nullptr;

//     if (!(flags & MapTemplateInstantiations) && D->isTemplateInstantiation() &&
//             D->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization)
//         return nullptr;

    auto loc = fromLoc(D->getLocation());
    auto MD = dyn_cast<clang::CXXMethodDecl>(D);
    auto CCD = dyn_cast<clang::CXXConstructorDecl>(D);

//     if (D->isInvalidDecl())
//         return nullptr;

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
            auto R = getDeclContextOpaque(D)->lookup(D->getDeclName());

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
        setDsym(D, fd);
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

    setDsym(D, fd);

    if (D->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization &&
            D->getPrimaryTemplate() // forward-declared explicit specializations do not have their primary template set (stangely)
            /*&& !(flags & MapTemplateInstantiations)*/)
    {
        auto tpl = new TemplateParameters;

        auto FT = D->getPrimaryTemplate();
        auto TPL = FT->getTemplateParameters();
        auto AI = D->getTemplateSpecializationArgs()->asArray().begin();

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
    else if (isTemplateInstantiation(D) &&
             D->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization &&
             (flags & CreateTemplateInstance))
        a = CreateTemplateInstanceFor(D, a);

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

    for (auto P : *TPL)
    {
        auto tp = VisitTemplateParameter(P);
        if (!tp)
            return nullptr; // should be extremely rare, e.g if there's a int128_t value parameter
        tpl->push(tp);
    }

    auto td = new TemplateDeclaration(loc, id, tpl, new Dsymbols, D);
    setDsym(D, td);

    // FIXME: function overloads with volatile parameters?

    auto a = new Dsymbols;
    a->push(td);

    if (false) // NOTE: mapping explicit and partial specs isn't needed since cpp::TemplateInstance
               // will route to the correct spec. And there might a massive amount of explicit specs
               // in some cases (such as Qt's QTypeInfo), so mapping them lazily is a *huge* win.
    {
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
                tp_defaultvalue = expmap.fromExpression(NTTPD->getDefaultArgument());

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

    for (auto PI = TPL->begin(), PE = TPL->end();
        PI != PE; PI++)
    {
        if (AI && isTemplateParameterPack(*PI)
                && AI->getKind() == clang::TemplateArgument::Pack
                && AI->pack_size() == 0)
            break; // ex.: std::tuple<> explicit spec of std::tuple<Elem..> // FIXME this doesn't work if there's an argument after a pack

        auto tp = VisitTemplateParameter(*PI, AI);
        if (!tp)
            return nullptr;
        tpl->push(tp);

        if (AI) AI++;
    }

    if (Partial)
        // SEMI-HACK: even if we don't map partial specialization arguments, we still need to call fromTemplateArgument() to import module dependencies
        for (auto& SpecArg: D->getTemplateArgs().asArray())
            FromType(*this, loc).fromTemplateArgument(&SpecArg);

    auto decldefs = VisitSpecDecl(*this, D);
    if (!decldefs)
        return nullptr;

    auto s = new Dsymbols;
    auto td = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    s->push(td);

    if (!Partial)
    {
        auto tiargs = new Objects;
        for (auto tp: *tpl)
            tiargs->push(tp->specialization());

        auto explicit_ti = new TemplateInstance(loc, td->ident, tiargs);
        explicit_ti->tempdecl = td;
        explicit_ti->havetempdecl = true;
        explicit_ti->semantictiargsdone = true;
        explicit_ti->isForeignInst = true;
        explicit_ti->Inst = const_cast<SpecTy*>(D);
        explicit_ti->members = decldefs; // NOTE: it doesn't matter that td and ti share the same members, semantic(td) doesn't do any change to them
        explicit_ti->tdtypes.setDim(tiargs->dim);
        memcpy(explicit_ti->tdtypes.data, tiargs->data, tiargs->dim * sizeof(void*));
        s->push(explicit_ti);
    }

    return s;
}

// WARNING: this is for explicit or partial specs, this might need a better name
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
    auto CanonDecl = D;
    if (!D->isCompleteDefinition())
        D = D->getDefinition();

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
    setDsym(CanonDecl, e);

    return oneSymbol(e);
}

Dsymbols *DeclMapper::VisitEnumConstantDecl(const clang::EnumConstantDecl *D)
{
    auto memberLoc = fromLoc(D->getLocation());
    auto ident = fromIdentifier(D->getIdentifier());

    auto parent = static_cast<EnumDeclaration*>(
                dsymForDecl(cast<clang::Decl>(getDeclContextOpaque(D))));

    Expression *value = nullptr;

    if (auto InitE = D->getInitExpr())
    {
        value = ExprMapper(*this).fromExpression(InitE);
        value = new_CastExp(memberLoc, value, parent->memtype); // SEMI-HACK (?)
                                // the type returned by 1LU << ... will be ulong yet
                                // we may need an int (see wctype.h
    }

    auto em = new EnumMember(memberLoc, ident, value, nullptr, D);
    setDsym(D, em);
    em->ed = parent;

    return oneSymbol(em);
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

Dsymbol* DeclMapper::dsymForDecl(const clang::Decl* D)
{
    if (auto ND = dyn_cast<clang::NamedDecl>(D))
        return dsymForDecl(ND);

    assert(isa<clang::TranslationUnitDecl>(D) && "Unhandled dsymForDecl(clang::Decl)");
    return Module::get(D);
}

Dsymbol* DeclMapper::dsymForDecl(const clang::NamedDecl* D)
{
    D = cast<clang::NamedDecl>(getCanonicalDecl(D));

    if (D->d)
        return D->d->sym;

    if (auto NS = dyn_cast<clang::NamespaceDecl>(D)) {
        if (NS->isInlineNamespace())
            return dsymForDecl(cast<clang::Decl>(NS->getDeclContext()));
        return Module::get(NS);
    }

    ScopeDsymbol* parent = nullptr;

    auto Parent = cast<clang::Decl>(getDeclContextOpaque(D));
    bool IsParentNamespaceOrTU = isa<clang::TranslationUnitDecl>(Parent) ||
                    isa<clang::NamespaceDecl>(Parent);

    if (IsParentNamespaceOrTU)
    {
        auto Func = dyn_cast<clang::FunctionDecl>(D);
        if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
            Func = FuncTemp->getTemplatedDecl();

        if (Func && Func->isOverloadedOperator())
        {
            if (auto Tag = isOverloadedOperatorWithTagOperand(D))
                parent = Module::get(Tag); // non-member operators are part of the record module
        }
        else if (isa<clang::TagDecl>(D) || isa<clang::ClassTemplateDecl>(D))
            parent = Module::get(D);
        else
            parent = Module::get(Parent); // aka _
    }
    else
    {
        auto s = dsymForDecl(Parent);
        assert(s->isScopeDsymbol());
        parent = static_cast<ScopeDsymbol*>(s);
    }

    auto minst = this->minst;
    if (auto ti = parent->isInstantiated())
        minst = ti->minst;

    DeclMapper(minst, parent->getModule()->importedFrom).VisitDecl(D,
                    DeclMapper::CreateTemplateInstance);
    assert(D->d);

    if (!isa<clang::IndirectFieldDecl>(D))
        parent->members->push(D->d->sym);
    D->d->sym->addMember(nullptr, parent);

    return D->d->sym;
}

// ***** //

Module *DeclMapper::getModule(const clang::Decl* rootDecl)
{
    Module* m = Module::allCppModules[rootDecl];

    if (!m)
    {
        auto parent = getPackage(rootDecl);

        bool IsNamespaceOrTU = isa<clang::TranslationUnitDecl>(rootDecl) ||
                isa<clang::NamespaceDecl>(rootDecl);
        auto ident = IsNamespaceOrTU ? calypso.id__ : getIdentifier(cast<clang::NamedDecl>(rootDecl));

        llvm::SmallString<24> objFilename("__cpp-");
        for (auto pkg = parent; pkg != Module::rootPackage; pkg = static_cast<Package*>(pkg->parent))
        {
            objFilename += pkg->ident->toChars();
            objFilename += "-";
        }
        objFilename += ident->toChars();

        auto m = new Module(strdup(objFilename.c_str()), ident);
        m->rootDecl = rootDecl;
        m->importedFrom = importedFrom;
        Module::allCppModules[rootDecl] = m;

        m->loadEmittedSymbolList();

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
            m->objfile = setOutCalypsoFile(global.params.objdir, m->arg, objExt);
    }

    return m;
}

Package *DeclMapper::getPackage(const clang::Decl* rootDecl)
{
    if (isa<clang::TranslationUnitDecl>(rootDecl))
        return Module::rootPackage;

    auto Parent = cast<clang::Decl>(getDeclContextOpaque(rootDecl));

    if (isa<clang::TagDecl>(rootDecl) || isa<clang::ClassTemplateDecl>(rootDecl))
        return getPackage(Parent);

    auto NS = cast<clang::NamespaceDecl>(rootDecl);
    if (NS->d) {
        assert(NS->d->sym->isPackage());
        return static_cast<Package*>(NS->d->sym);
    }

    auto parent = getPackage(Parent);

    auto pkg = new_Package(getIdentifier(NS));
    setDsym(NS, pkg);
    parent->symtab->insert(pkg);
    pkg->parent = parent;
    pkg->symtab = new_DsymbolTable();

    return pkg;
}

Dsymbol* dsymForDecl(ScopeDsymbol* sds, const clang::Decl* D)
{
    return DeclMapper(sds).dsymForDecl(D);
}

// ***** //

Module *Module::load(Loc loc, Identifiers *packages, Identifier *id/*, bool& isTypedef*/)
{
    if (!calypso.getASTUnit()) {
        ::error(loc, "Importing a C++ module without specifying C++ headers with pragma(cppmap, \"...\") first");
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

    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];

        auto R = DC->lookup(calypso.toDeclarationName(pid));
        if (R.empty())
        {
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

    isTypedef = false;

    const clang::Decl *rootDecl, *D = nullptr;

    if (id == calypso.id__)
    {
        rootDecl = D = cast<clang::Decl>(DC)->getCanonicalDecl();
    }
    else
    {
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
            auto R = DC->lookup(calypso.toDeclarationName(id));
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
                        isTypedef = true; // FIXME LAZY APPROACH
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
                         "Importing a typedef is equivalent to \"import (C++) _ : 'typedef';\"\n"
                         "(Typedefs are included in the _ module)");
            fatal();
        }

        D = cast<clang::NamedDecl>(const_cast<clang::Decl*>(getCanonicalDecl(D)));

        if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
            rootDecl = CTD->getTemplatedDecl();
        else
            rootDecl = D;
    }

    auto m = allCppModules[rootDecl];
    if (!m)
        m = create(rootDecl, packages, id);
    m->parent = pkg;
    m->loc = loc;
    m->members = new Dsymbols;

    amodules.push_back(m);
    pkg->symtab->insert(m);

    if (id == calypso.id__)
    {
        if (isa<clang::TranslationUnitDecl>(DC))
            for (auto& P: calypso.MacroMap)
                if (auto s = m->mapper->VisitMacro(P.first, P.second))
                    m->members->push(s);

        m->searchInlineNamespaces();
    }
    else
    {
        if (auto s = dsymForDecl(D))
            m->members->push(s);
    }
    // the rest of the module is mapped lazily

    return m;
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

static inline bool isTopLevelInNamespaceModule (const clang::Decl *D)
{
    auto Tag = dyn_cast<clang::TagDecl>(D);
    if (Tag && (Tag->getIdentifier() || Tag->getTypedefNameForAnonDecl()))
        return false; // anonymous tags are added to _ as well

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

static void mapNamespace(Module* m, const clang::DeclContext *DC)
{
    llvm::SmallVector<clang::DeclContext*, 1> Ctxs;
    const_cast<clang::DeclContext*>(DC)->collectAllContexts(Ctxs);

    for (auto Ctx: Ctxs)
    {
        auto CanonCtx = cast<clang::Decl>(Ctx)->getCanonicalDecl();

        for (auto D = Ctx->decls_begin(), DE = Ctx->decls_end(); D != DE; ++D)
        {
            if (cast<clang::Decl>(D->getDeclContext())->getCanonicalDecl() != CanonCtx)
                continue;  // only map declarations that are semantically within the DeclContext

            auto InnerNS = dyn_cast<clang::NamespaceDecl>(*D);
            if ((InnerNS && InnerNS->isInline()) || isa<clang::LinkageSpecDecl>(*D))
            {
                mapNamespace(m, cast<clang::DeclContext>(*D));
                continue;
            }
            else if (!isTopLevelInNamespaceModule(*D))
                continue;

            addToMembers(m, *D);
        }
    }
}

void Module::complete()
{
    members->setDim(0);

    if (ident == calypso.id__)
    { // Hardcoded module with all the top-level non-tag decls + the anonymous tags of a namespace
        mapNamespace(this, cast<clang::DeclContext>(rootDecl));
    }
    else if (auto Tag = dyn_cast<clang::TagDecl>(rootDecl))
    {
        // Sometimes friend declarations inside a record are the only existing declarations,
        // so map them to the record's parent module.
        // see friend QString::operator==(const QString &s1, const QString &s2);
        typedef clang::DeclContext::specific_decl_iterator<clang::FriendDecl> Friend_iterator;
        for (Friend_iterator I(Tag->decls_begin()), E(Tag->decls_end()); I != E; I++)
        {
            auto Decl = (*I)->getFriendDecl();
            if (!Decl || !Decl->isOutOfLine())
                continue;

            auto DeclCtx = dyn_cast<clang::DeclContext>(Decl);
            if (DeclCtx && DeclCtx->isDependentContext())
                continue;

            addToMembers(Decl);
        }

        // Add the non-member overloaded operators that are meant to work with this record/enum
        for (int I = 1; I < clang::NUM_OVERLOADED_OPERATORS; I++)
        {
            auto Op = static_cast<clang::OverloadedOperatorKind>(I);
            searchNonMemberOverloadedOperators(Op);

            for (auto OO: nonMemberOverloadedOperators[Op].OOs)
                addToMembers(OO);
        }
    }
}

void Module::searchInlineNamespaces()
{
    if (searchedInlineNamespaces)
        return;
    searchedInlineNamespaces = true;

    auto DC = cast<clang::DeclContext>(rootDecl);

    typedef clang::DeclContext::specific_decl_iterator<clang::NamespaceDecl> Namespace_iterator;
    for (Namespace_iterator I(DC->decls_begin()), E(DC->decls_end()); I != E; I++)
        if ((*I)->isInlineNamespace() && (*I)->isOriginalNamespace())
            inlineNamespaces.push_back(*I);
}

bool isRecordMemberInModuleContext(clang::Decl* D)
{
    auto Friend = dyn_cast<clang::FriendDecl>(D);
    if (!D)
        return false;

    auto Friended = Friend->getFriendDecl();
    if (!Friended || !Friended->isOutOfLine())
        return false;

    auto FriendDC = dyn_cast<clang::DeclContext>(Friended);
    if (FriendDC && FriendDC->isDependentContext())
        return false; // FIXME: dependent out-of-line free operators defined in a record as friend decl
                      // (ex. piecewise_linear_distribution::operator==) do not get added to the parent
                      // declcontext, so aren't mapped to the module

    return true;
}

Dsymbol *Module::search(const Loc& loc, Identifier *ident, int flags)
{
    if (auto s = symtab->lookup(ident))
        return s;

    auto DC = cast<clang::DeclContext>(rootDecl);
    auto Name = calypso.toDeclarationName(ident);

    auto R = DC->lookup(Name);

    if (this->ident == calypso.id__)
    {
        for (auto Match: R)
            if (isTopLevelInNamespaceModule(Match))
                addToMembers(this, Match);

        for (auto NS: inlineNamespaces)
            for (auto Match: NS->lookup(Name))
                if (isTopLevelInNamespaceModule(Match))
                    addToMembers(this, Match);

        return symtab->lookup(ident);
    }
    else
    {
        for (auto Match: R)
            if (isRecordMemberInModuleContext(Match))
                addToMembers(this, Match);

        if (Name.getNameKind() == clang::DeclarationName::CXXOperatorName)
        {
            auto Op = Name.getCXXOverloadedOperator();
            searchNonMemberOverloadedOperators(Op);

            for (auto OO: nonMemberOverloadedOperators[Op].OOs)
                addToMembers(this, OO);
        }

        return symtab->lookup(ident);
    }
}

void Module::searchNonMemberOverloadedOperators(clang::OverloadedOperatorKind Op)
{
    if (nonMemberOverloadedOperators[Op].searched)
        return;
    nonMemberOverloadedOperators[Op].searched = true;

    auto RD = dyn_cast<clang::RecordDecl>(rootDecl);
    if (!RD)
        return;

    auto& Context = calypso.getASTContext();

    auto OpName = Context.DeclarationNames.getCXXOperatorName(
                static_cast<clang::OverloadedOperatorKind>(Op));

    for (auto Ctx = rootDecl->getDeclContext(); Ctx; Ctx = Ctx->getLookupParent())
    {
        if (Ctx->isTransparentContext())
            continue;

        for (auto OverOp: Ctx->lookup(OpName))
        {
            if (!isOverloadedOperatorWithTagOperand(OverOp, cast<clang::NamedDecl>(rootDecl)))
                continue;

            if (OverOp->getFriendObjectKind() != clang::Decl::FOK_None && OverOp->isOutOfLine())
                continue; // friend out-of-line decls are already mapped as part as the record

            nonMemberOverloadedOperators[Op].OOs.push_back(getCanonicalDecl(OverOp));
        }
    }
}

}
