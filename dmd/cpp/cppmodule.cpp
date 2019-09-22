// Contributed by Elie Morisse, same license DMD uses

#include "aggregate.h"
#include "attrib.h"
#include "declaration.h"
#include "enum.h"
#include "expression.h"
#include "id.h"
#include "identifier.h"
#include "import.h"
#include "init.h"
#include "identifier.h"
#include "scope.h"
#include "statement.h"
#include "template.h"
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

#include "llvm/Support/raw_ostream.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/RecursiveASTVisitor.h"
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
std::map<const clang::Decl*, Module*> Module::allCppModules;
Modules Module::amodules_cpp;

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
        os << symname.first() << "\n";
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

inline Prot::Kind fromProt(clang::AccessSpecifier AS)
{
    switch(AS) {
        case clang::AS_public:
            return Prot::public_;
        case clang::AS_protected:
            return Prot::protected_;
        case clang::AS_private:
            return Prot::private_;
        case clang::AS_none:
            return Prot::/*none*/public_;
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
    auto D_ = const_cast<clang::NamedDecl*>(D);

    if (!D->d)
        D_->d = new DData;
    assert(D->d->sym == nullptr);
    D_->d->sym = sym;
    D_->d->hasSym = true;
}

inline void setDwrapper(const clang::NamedDecl* D, TemplateDeclaration* wrapper)
{
    if (!D->d)
        const_cast<clang::NamedDecl*>(D)->d = new DData;
    assert(D->d->wrapper == nullptr);
    const_cast<clang::NamedDecl*>(D)->d->wrapper = wrapper;
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
    DECL(Enum)
    DECL(EnumConstant)
    DECLWF(Record)
    DECLWF(Function)
    DECLWF(Value)
    DECL(RedeclarableTemplate)

#undef DECL
#undef DECLWF

    return s;
}

Dsymbols *DeclMapper::VisitPartialOrWrappedDecl(const clang::Decl *D, unsigned flags)
{
    if (D != getCanonicalDecl(D))
        return nullptr;

    assert(flags & WrapExplicitSpecsAndOverloadedOperators);

    Dsymbols *s = nullptr;

#define DECLSPEC(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) { \
        if (flags & MapExplicitAndPartialSpecs) \
            s = Visit##BASE##Decl(BASE##D); }
#define DECLWF(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D, flags);

    if (0) ;
    DECLSPEC(ClassTemplateSpecialization)
    DECLSPEC(VarTemplateSpecialization)
    DECLWF(Function)

#undef DECLSPEC
#undef DECLWF

    return s;
}

template<typename SpecTy>
Dsymbols* DeclMapper::CreateTemplateInstanceFor(const SpecTy* D, Dsymbols* decldefs)
{
    auto TempDecl = cast<clang::NamedDecl>(getSpecializedDeclOrExplicit(D));
    auto PrimParams = getTemplateParameters(getPrimaryTemplate(TempDecl));
    auto PrimArgs = getTemplateArgs(D);

    auto tempdecl = templateForDecl(TempDecl);
    assert(tempdecl && tempdecl->isTemplateDeclaration());
    auto c_td = static_cast<TemplateDeclaration*>(tempdecl);

    auto loc = (*decldefs)[0]->loc;
    auto tiargs = fromTemplateArguments<false>(loc, PrimArgs, PrimParams);
    auto ti = new TemplateInstance(loc, static_cast<TemplateDeclaration*>(tempdecl), tiargs);
    ti->isForeignInst = true;
    ti->inst = ti;
    ti->Inst = const_cast<SpecTy*>(cast<SpecTy>(getCanonicalDecl(D)));
    ti->minst = minst;

    auto cpptdtypes = c_td->tdtypesFromInst(minst ? minst->_scope : nullptr, ti->Inst, false);
        // NOTE: minst may be null for speculative instances, e.g from hasCopyCtor()
    ti->tdtypes.setDim(cpptdtypes->dim);
    memcpy(ti->tdtypes.tdata(), cpptdtypes->tdata(), cpptdtypes->dim * sizeof(void*));
    delete cpptdtypes;

    ti->correctTiargs(); // set ti->primTiargs

    ti->parent = tempdecl->parent;
    ti->members = decldefs;
    ti->symtab = new_DsymbolTable();
    for (auto s: *decldefs)
        s->addMember(nullptr, ti);

    if (decldefs->dim == 1)
        ti->aliasdecl = (*decldefs)[0];

    ti->semantictiargsdone = true;
    ti->semanticRun = PASSsemantic3done;

    assert(!c_td->findExistingInstance(ti, nullptr));

    if (ti->minst)
    {
        c_td->addInstance(ti);
        ti->appendToModuleMember();
    }

    decldefs = new Dsymbols;
    decldefs->push(ti);

    return decldefs;
}

Dsymbols *DeclMapper::VisitValueDecl(const clang::ValueDecl *D, unsigned flags)
{
    auto CanonDecl = D;

    assert(!isa<clang::IndirectFieldDecl>(D));

    if (isa<clang::VarTemplatePartialSpecializationDecl>(D))
        return nullptr;

    if (auto Var = dyn_cast<clang::VarDecl>(D))
    {
        assert(!Var->getDescribedVarTemplate());

        if (auto Def = Var->getDefinition())
            D = Def;
    }

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
    auto t = fromType(D->getType(), loc);

    if (!t)
        return nullptr;

//     if (t->isConst())
//         t = t->immutableOf();

    auto a = new VarDeclaration(loc, id, D, t);
    setDsym(CanonDecl, a);

    a->linkage = LINKcpp;
    a->protection.kind = fromProt(D->getAccess());
    // a->alignment =?

    if (t->isConst())
        a->storage_class |= STCconst;

    const clang::Expr* Init = nullptr;

    if (auto Var = dyn_cast<clang::VarDecl>(D))
    {
        if (Var->hasExternalStorage())
            a->storage_class |= STCextern;

        if (Var->getTLSKind() == clang::VarDecl::TLS_Dynamic)
            a->storage_class |= STCtls;

        if (Var->isStaticDataMember())
            a->storage_class |= STCstatic;

        if ((Var->isConstexpr() || t->isConst()) &&
                Var->getAnyInitializer())
            Init = Var->getAnyInitializer(); // we avoid initializer expressions
                                             // except for const/constexpr variables
    }
    else if (auto Field = dyn_cast<clang::FieldDecl>(D))
        Init = Field->getInClassInitializer();

    if (Init)
    {
        auto& Context = calypso.getASTContext();
        ExprMapper expmap(*this);

        clang::Expr::EvalResult Result;

        Expression *e = nullptr;
        if (!Init->isValueDependent() && Init->EvaluateAsConstantExpr(Result,
                                                clang::Expr::EvaluateForCodeGen, Context))
            e = expmap.fromAPValue(loc, Result.Val, D->getType());
                // NOTE: Evaluating integer and boolean expressions is always preferable,
                // because in some rare cases DMD and Clang's intepretations differ,
                // one important instance being -1u < 0u (true for DMD, false for Clang)

        if (!e)
            e = expmap.fromExpression(Init, true);

        if (e && e->op != TOKnull)
            if (Init->isInstantiationDependent() || Init->isEvaluatable(Context))
                a->_init = new_ExpInitializer(loc, e);
    }

    a->semanticRun = PASSsemantic3done;
    decldefs->push(a);

    auto VarSpec = dyn_cast<clang::VarTemplateSpecializationDecl>(D);
    if (VarSpec && (flags & CreateTemplateInstance))
        CreateTemplateInstanceFor(VarSpec, decldefs);

    return decldefs;
}

// For simplicity's sake (or confusion's) let's call records with either virtual functions or bases polymorphic
bool isPolymorphic(const clang::RecordDecl *D)
{
    if (!D->isCompleteDefinition())
    {
        if (D->getDefinition())
            D = D->getDefinition();
        else if (auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
            if (!CTSD->isExplicitSpecialization())
            {
                const clang::RecordDecl* SpecRecord;

                auto U = CTSD->getSpecializedTemplateOrPartial();
                if (U.is<clang::ClassTemplateDecl*>())
                    SpecRecord = U.get<clang::ClassTemplateDecl*>()->getTemplatedDecl();
                else
                    SpecRecord = U.get<clang::ClassTemplatePartialSpecializationDecl*>();

                return isPolymorphic(SpecRecord);
            }
    }

    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
    return CRD && D->isCompleteDefinition() &&
                (CRD->getNumBases() || CRD->isPolymorphic());
}

Dsymbols *DeclMapper::VisitRecordDecl(const clang::RecordDecl *D, unsigned flags)
{
    auto CanonDecl = D;

    if (!isMapped(D))
        return nullptr;

    if (D->isInjectedClassName())
        return nullptr;

    if (isa<clang::ClassTemplatePartialSpecializationDecl>(D))
        return nullptr;

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

    // NOTE: the special case of union {} myUnion; has no real D equivalent
    // but we simply map it as a true anonymous AggregateDeclaration

    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
    auto members = new Dsymbols;

    assert(!CRD || !CRD->getDescribedClassTemplate());

    AggregateDeclaration *a;
    if (!anon)
    {
        auto id = getIdentifierOrNull(D);

        if (D->isUnion())
        {
            a = new UnionDeclaration(loc, id, D);
        }
        else if (!isPolymorphic(D))
        {
            auto sd = new StructDeclaration(loc, id, D);
            sd->hasIdentityAssign = true; // currently DMD only checks hasIdentityAssign before attempting to
                // overload an identity assignment, but even if true this is followed by a resolveFuncCall anyway
            a = sd;
        }
        else
        {
            a = new ClassDeclaration(loc, id, /*baseclasses =*/ nullptr, members, CRD);
            // NOTE: baseclasses are being mapped lazily during buildVtbl
        }

        setDsym(CanonDecl, a);

        a->protection.kind = Prot::public_;
        a->semanticRun = PASSsemantic3done;
        a->symtab = new_DsymbolTable();
    }

    if (anon)
        decldefs->push(new AnonDeclaration(loc, anon == 2, members));
    else
    {
        a->members = members;
        decldefs->push(a);

        auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D);
        if (ClassSpec && (flags & CreateTemplateInstance))
            CreateTemplateInstanceFor(ClassSpec, decldefs);
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

    auto a = new AliasDeclaration(loc, id, Type::tvoid, D);
    a->type = nullptr; // Type::tvoid was to get past the ctor's assert
    setDsym(D, a);
    a->semanticRun = PASSsemantic3done;
    return oneSymbol(a);
}

TemplateParameters *initTempParams(Loc loc, SpecValue &spec)
{
    auto tpl = new TemplateParameters;
    TemplateParameter *p = nullptr;

    if (spec.op)
    {
        auto tp_specvalue = new_StringExp(loc, const_cast<char*>(spec.op));
        p = new_TemplateValueParameter(loc, calypso.id_op,
                                        Type::tstring, tp_specvalue, nullptr);
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

inline Dsymbol* dummyOnemember(Loc loc, Identifier* ident)
{
    auto tf = new_TypeFunction(nullptr, Type::tvoid, 0, LINKd);

    if (ident == Id::ctor)
        return new_CtorDeclaration(loc, loc, 0, tf);
    else
        return new_FuncDeclaration(loc, loc, ident, 0, tf);
}

Dsymbols *DeclMapper::VisitFunctionDecl(const clang::FunctionDecl *D, unsigned flags)
{
    if (!isMapped(D))
        return nullptr;

    if (D->getDescribedFunctionTemplate())
        return nullptr;

//     if (!(flags & MapTemplateInstantiations) && D->isTemplateInstantiation() &&
//             D->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization)
//         return nullptr;

    auto loc = fromLoc(D->getLocation());
    auto FPT = D->getType()->castAs<clang::FunctionProtoType>();

    auto tf = FromType(*this, loc).fromTypeFunction(FPT, D);
    if (!tf)
    {
        if (opts::cppVerboseDiags)
            ::warning(loc, "Discarding %s, non-supported argument or return type (e.g int128_t)",
                                D->getDeclName().getAsString().c_str());
        return nullptr;
    }
    assert(tf->ty == Tfunction);

    if (flags & WrapExplicitSpecsAndOverloadedOperators)
    {
        if (D->isTemplateInstantiation() && D->getTemplateSpecializationInfo()) // i.e is instantiated but not from a template member
            return nullptr;

        TemplateParameters* tpl = nullptr;

        SpecValue spec(*this);
        auto ident = getIdentifierOrNull(D, &spec); // will return nullptr if this is an overloaded operator not supported by D

        // NOTE: C++ overloaded operators might be virtual, unlike D which are always final (being templates)
        // Mapping the C++ operator to opBinary()() directly would make D lose info and overriding the C++ method impossible

        if (!ident)
            return nullptr; // TODO map the unsupported operators anyway

        if (spec)
        {
            assert(D->isOverloadedOperator() || isa<clang::CXXConversionDecl>(D));
            tpl = initTempParams(loc, spec);
        }

        if (D->getPrimaryTemplate())
        {
            assert(D->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization);
            // NOTE: forward-declared explicit specializations do not have their primary template set (stangely)

            if (!tpl)
                tpl = new TemplateParameters;

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
        }

        if (!tpl)
            return nullptr;

        auto td = new TemplateDeclaration(loc, ident, tpl, nullptr, D);
        setDwrapper(D, td);
        td->semanticRun = PASSsemantic3done;
        td->onemember = dummyOnemember(loc, ident); // HACK: Create a dummy oneMember for functionResolve
        return oneSymbol(td);
    }

    auto MD = dyn_cast<clang::CXXMethodDecl>(D);
    auto CCD = dyn_cast<clang::CXXConstructorDecl>(D);

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

    ::FuncDeclaration *fd;

    if (auto CD = dyn_cast<clang::CXXConstructorDecl>(D))
    {
        fd = new CtorDeclaration(loc, stc, tf, CD);
    }
    else if (auto DD = dyn_cast<clang::CXXDestructorDecl>(D))
    {
        fd = new DtorDeclaration(loc, stc, Id::dtor, DD);
    }
    else
    {
        auto id = getExtendedIdentifier(D, *this);
        fd = new FuncDeclaration(loc, id, stc, tf, D);
    }

    setDsym(D, fd);

    fd->linkage = LINKcpp;
    fd->protection.kind = fromProt(D->getAccess());
    fd->originalType = tf;

    fd->semanticRun = PASSsemantic3done;

    if ((isTemplateInstantiation(D) || isExplicitSpecialization(D)) &&
             D->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization &&
             (flags & CreateTemplateInstance))
        CreateTemplateInstanceFor(D, oneSymbol(fd));

    return oneSymbol(fd);
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

    td->semanticRun = PASSsemantic3done;

    // HACK: Create a dummy oneMember for functionResolve
    if (isa<clang::FunctionTemplateDecl>(D))
        td->onemember = dummyOnemember(loc, id);

    auto a = new Dsymbols;
    a->push(td);

    // FIXME: function overloads with volatile parameters?

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

//             if (NTTPD->hasDefaultArgument()) // LAZY NOTE: default args should never be needed, since they're evaluated by matchWithInstance
//                 tp_defaultvalue = expmap.fromExpression(NTTPD->getDefaultArgument());

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

//             if (TTPD->hasDefaultArgument())
//                 tp_defaulttype = fromType(TTPD->getDefaultArgument(), loc);

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

//             if (TempTemp->hasDefaultArgument())
//                 tp_defaulttype = FromType(*this, loc).fromTemplateName(
//                         TempTemp->getDefaultArgument().getArgument().getAsTemplate());

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

    // NOTE: D's partial specializations != C++'s partial specializations
    // The mapping provides a "close" but not exact approximation of equivalent template specs in D (for reflection),
    // but TemplateDeclaration::findBestMatch is skipped since the choice is done by Clang anyway.

    auto Partial = dyn_cast<PartialTy>(D);

    assert(Partial); // NOTE: mapping explicit specs to TemplateDeclaration is disabled,
                     // to simplify the handling of C++ templates and because at the time
                     // of writing it wouldn't add anything to reflection

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

    auto td = new TemplateDeclaration(loc, id, tpl, new Dsymbols, D);
    td->semanticRun = PASSsemantic3done;
    setDwrapper(D, td);
    return oneSymbol(td);
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
    e->members = new Dsymbols;
    e->symtab = new_DsymbolTable();
    e->semanticRun = PASSsemantic3done;

    return oneSymbol(e);
}

Dsymbols *DeclMapper::VisitEnumConstantDecl(const clang::EnumConstantDecl *D)
{
    auto loc = fromLoc(D->getLocation());
    auto ident = fromIdentifier(D->getIdentifier());

    auto parent = dsymForDecl(cast<clang::Decl>(D->getDeclContext()));
    assert(parent->isEnumDeclaration());
    auto value = ExprMapper(*this).fromAPInt(loc, D->getInitVal(), clang::QualType());

    auto em = new EnumMember(loc, ident, value, nullptr, D);
    setDsym(D, em);

    em->ed = static_cast<EnumDeclaration*>(parent);
    em->storage_class |= STCmanifest;
    em->type = em->ed->type;
    em->semanticRun = PASSsemantic3done;

    return oneSymbol(em);
}

/*****/

template <unsigned flags>
Dsymbol* DeclMapper::dsymForDecl(const clang::Decl* D)
{
    if (auto ND = dyn_cast<clang::NamedDecl>(D))
        return dsymForDecl<flags>(ND);

    assert(isa<clang::TranslationUnitDecl>(D) && "Unhandled dsymForDecl(clang::Decl)");
    return getModule(D);
}

template <unsigned flags>
Dsymbol* DeclMapper::dsymForDecl(const clang::NamedDecl* D)
{
    D = cast<clang::NamedDecl>(getCanonicalDecl(D));
    bool wantPartialOrWrappedDecl = flags & DeclMapper::WrapExplicitSpecsAndOverloadedOperators;

    if (wantPartialOrWrappedDecl)
    {
        if (!D->d || !D->d->hasSym)
            dsymForDecl<NoFlag>(D);
        if (D->d->wrapper)
            return D->d->wrapper; // NOTE: hence if null because unsupported it will go through dsymForDecl again
    }
    else if (D->d && D->d->hasSym)
        return D->d->sym;

    if (auto NS = dyn_cast<clang::NamespaceDecl>(D))
    {
        if (NS->isInlineNamespace())
            return dsymForDecl<NoFlag>(cast<clang::Decl>(NS->getDeclContext()));
        return getModule(NS);
    }

    ScopeDsymbol* parent = nullptr;

    auto Parent = cast<clang::Decl>(getDeclContextOpaque(D));
    bool IsParentNamespaceOrTU = isa<clang::TranslationUnitDecl>(Parent) ||
                    isa<clang::NamespaceDecl>(Parent);

    if (IsParentNamespaceOrTU)
    {
        auto ModDecl = D;

        auto Func = dyn_cast<clang::FunctionDecl>(D);
        if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
            Func = FuncTemp->getTemplatedDecl();

        if (Func && Func->isOverloadedOperator())
            if (auto Tag = isOverloadedOperatorWithTagOperand(D))
                ModDecl = D; // non-member operators are part of the record module

        if (auto CRD = dyn_cast<clang::CXXRecordDecl>(ModDecl))
            if (auto Template = CRD->getDescribedClassTemplate())
                ModDecl = Template;

        if (isa<clang::TagDecl>(ModDecl) || isa<clang::ClassTemplateDecl>(ModDecl))
            parent = getModule(ModDecl);
        else
            parent = getModule(Parent); // aka _
    }
    else
    {
        auto s = dsymForDecl<NoFlag>(Parent);
        assert(s->isScopeDsymbol());
        parent = static_cast<ScopeDsymbol*>(s);
    }

    if (!wantPartialOrWrappedDecl)
        VisitDecl(D, flags | DeclMapper::CreateTemplateInstance);
    else
        VisitPartialOrWrappedDecl(D, flags);

    if (!D->d || !D->d->hasSym)
    {
        assert(!wantPartialOrWrappedDecl);
        setDsym(D, nullptr);
        return nullptr;
    }

    auto sym = wantPartialOrWrappedDecl ? D->d->wrapper : D->d->sym;
    if (!sym)
        return nullptr;

    if (!sym->parent)
    {
        parent->members->push(sym);
        sym->addMember(nullptr, parent);

        auto ad = static_cast<AggregateDeclaration*>(parent);
        if (sym->ident == Id::ctor)
        {
            assert(parent->isAggregateDeclaration());
            if (!ad->ctor)
                ad->ctor = sym;

            if (auto ctor = sym->isCtorDeclaration())
            {
                auto tf = static_cast<TypeFunction*>(ctor->type);
                if (Parameter::dim(tf->parameters) == 0 && tf->varargs == 0)
                    ad->defaultCtor = ctor;
            }
        }
        else if (auto dtor = sym->isDtorDeclaration())
        {
            assert(parent->isAggregateDeclaration());
            if (!ad->dtor)
                ad->dtor = dtor;
        }

        if (sym->ident)
            parent->search(parent->loc, sym->ident, MapOverloads); // NOTE: this could be lazier..
    }
    else
        assert(sym->parent->isTemplateInstance() &&
                "Parent already set but not a TemplateInstance?");

    return sym;
}

template <unsigned flags>
Dsymbol* dsymForDecl(ScopeDsymbol* sds, const clang::Decl* D)
{
    return DeclMapper(sds).dsymForDecl<flags>(D);
}

template Dsymbol* dsymForDecl<DeclMapper::NoFlag>(ScopeDsymbol* sds, const clang::Decl* D);
template Dsymbol* dsymForDecl<DeclMapper::WrapExplicitSpecsAndOverloadedOperators>(ScopeDsymbol* sds, const clang::Decl* D);

void DeclMapper::dsymAndWrapperForDecl(const clang::Decl* D)
{
    if (auto Indirect = dyn_cast<clang::IndirectFieldDecl>(D))
        D = cast<clang::FieldDecl>(Indirect->chain().back());

    dsymForDecl(D);
    dsymForDecl<WrapExplicitSpecsAndOverloadedOperators>(D);
}

void dsymAndWrapperForDecl(ScopeDsymbol* sds, const clang::Decl* D)
{
    DeclMapper(sds).dsymAndWrapperForDecl(D);
}

Dsymbol* DeclMapper::templateForDecl(const clang::Decl* D)
{
    if (isa<clang::RedeclarableTemplateDecl>(D))
        return dsymForDecl(D);
    else
        return dsymForDecl<WrapExplicitSpecsAndOverloadedOperators|
                           MapExplicitAndPartialSpecs>(D);
}

Dsymbol* templateForDecl(ScopeDsymbol* sds, const clang::Decl* D)
{
    return DeclMapper(sds).templateForDecl(D);
}

void mapDecls(ScopeDsymbol* sds, const clang::DeclContext* DC, Identifier* ident)
{
    auto& DeclarationNames = calypso.getASTContext().DeclarationNames;
    DeclMapper mapper(sds);

    auto mapOperator = [&] (clang::OverloadedOperatorKind Op)
    {
        auto Name = DeclarationNames.getCXXOperatorName(Op);
        for (auto Match: DC->lookup(Name))
            mapper.dsymAndWrapperForDecl(Match);
    };

    if (ident == Id::opUnary)
        for (auto OO: {clang::OO_Plus, clang::OO_Minus, clang::OO_Star, clang::OO_Tilde,
                       clang::OO_PlusPlus, clang::OO_MinusMinus, clang::OO_Exclaim,
                       clang::OO_Arrow, clang::OO_ArrowStar})
            mapOperator(OO);
    else if (ident == Id::opBinary)
        for (auto OO: {clang::OO_Plus, clang::OO_Minus, clang::OO_Star, clang::OO_Slash,
                       clang::OO_Percent, clang::OO_Caret, clang::OO_Amp, clang::OO_Pipe,
                       clang::OO_Tilde, clang::OO_LessLess, clang::OO_GreaterGreater,
                       clang::OO_PlusPlus, clang::OO_MinusMinus, clang::OO_Comma})
            mapOperator(OO);
    else if (ident == Id::opOpAssign)
        for (auto OO: {clang::OO_PlusEqual, clang::OO_MinusEqual, clang::OO_StarEqual,
                       clang::OO_SlashEqual, clang::OO_PercentEqual, clang::OO_CaretEqual,
                       clang::OO_AmpEqual, clang::OO_PipeEqual, clang::OO_LessLessEqual,
                       clang::OO_GreaterGreaterEqual})
            mapOperator(OO);
    else if (ident == Id::cmp)
        /*mapOperator(clang::OO_EqualEqual)*/; // FIXME
        // NOTE: other overloaded operators only map to one DeclarationName so don't require
        // special treatment
    else if (ident == Id::_cast)
    {
        typedef clang::DeclContext::specific_decl_iterator<clang::CXXConversionDecl> Conv_iterator;
        for (Conv_iterator I(DC->decls_begin()), E(DC->decls_end()); I != E; I++)
            mapper.dsymAndWrapperForDecl(*I);

        typedef clang::DeclContext::specific_decl_iterator<clang::FunctionTemplateDecl> FuncTemp_iterator;
        for (FuncTemp_iterator I(DC->decls_begin()), E(DC->decls_end()); I != E; I++)
            if ((*I)->getDeclName().getNameKind() == clang::DeclarationName::CXXConversionFunctionName)
                mapper.dsymAndWrapperForDecl(*I);
    }
    else
    {
        auto Name = calypso.toDeclarationName(ident);

        for (auto Match: DC->lookup(Name))
            mapper.dsymAndWrapperForDecl(Match);
    }
}

// ***** //

Module *DeclMapper::getModule(const clang::Decl* D)
{
    if (auto ND = dyn_cast<clang::NamedDecl>(D))
        if (auto Prim = getPrimaryTemplate(ND))
            D = Prim;

    auto rootDecl = getCanonicalDecl(D);
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

        m = new Module(strdup(objFilename.c_str()), ident);
        m->rootDecl = rootDecl;
        m->parent = parent;
        m->importedFrom = importedFrom;
        m->loc = fromLoc(rootDecl->getLocation());
        m->members = new Dsymbols;
        m->symtab = new_DsymbolTable();
        m->setScope(Scope::createGlobal(m));
        m->semanticRun = PASSsemantic3done;

        Module::allCppModules[rootDecl] = m;
        Module::amodules_cpp.push(m);
        parent->symtab->insert(m);

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
        {
            m->objfile = setOutCalypsoFile(global.params.objdir, m->arg, objExt);
            if (!FileName::exists(m->objfile->toChars()))
                m->needGen = true;
        }
    }

    return m;
}

Package *DeclMapper::getPackage(const clang::Decl* D)
{
    if (isa<clang::TranslationUnitDecl>(D))
        return Module::rootPackage;

    auto Parent = cast<clang::Decl>(getDeclContextOpaque(D));

    if (isa<clang::TagDecl>(D) || isa<clang::ClassTemplateDecl>(D))
        return getPackage(Parent);

    auto NS = cast<clang::NamespaceDecl>(D);
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

// ***** //

void loadCppCore()
{
    Identifiers packages;
    packages.push(calypso.id_cpp);
    auto dst = Package::resolve(&packages, nullptr, nullptr);

    auto sym = dst->lookup(calypso.id_core);
    if (sym)
        return; // already loaded

    auto cpp_core_module = ::Module::load(Loc(), &packages, calypso.id_core);
    cpp_core_module->importedFrom = ::Module::rootModule;
    cpp_core_module->importAll(nullptr);
    dsymbolSemantic(cpp_core_module, nullptr);
    semantic2(cpp_core_module, nullptr);
    semantic3(cpp_core_module, nullptr);
}

Module *Module::load(Loc loc, Identifiers *packages, Identifier *id, bool& isTypedef)
{
    if (!calypso.getASTUnit()) {
        ::error(loc, "Importing a C++ module without specifying C++ headers with pragma(cppmap, \"...\") first");
        fatal();
    }

    loadCppCore();

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

    const clang::Decl *rootDecl = nullptr;

    if (id == calypso.id__)
    {
        rootDecl = cast<clang::Decl>(DC)->getCanonicalDecl();
    }
    else
    {
        // Lookups can't find the implicit __va_list_tag record
        if (packages->dim == 1)
        {
            if (id == calypso.id___va_list_tag)
                rootDecl = cast<clang::NamedDecl>(Context.getVaListTagDecl());
            else if (id == calypso.id___NSConstantString_tag)
                rootDecl = Context.getCFConstantStringTagDecl(); // FIXME: this isn't satisfying, problem #1: not future-proof, problem #2: platform-dependent
        }

        if (!rootDecl)
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
                        isTypedef = true;
                        return nullptr; // a new attempt will be made by cpp::Import::loadModule after fixing its id
                    }
                }

                if (isa<clang::TagDecl>(Match) || isa<clang::ClassTemplateDecl>(Match))
                {
                    rootDecl = Match;
                    break;
                }
            }
        }

        if (!rootDecl)
        {
            ::error(loc, "C++ modules have to be enums, records (class/struct, template or not), typedefs or _.\n"
                         "Importing a typedef is equivalent to \"import (C++) _ : 'typedef';\"\n"
                         "(Typedefs are included in the _ module)");
            fatal();
        }

        rootDecl = getCanonicalDecl(rootDecl);
    }

    auto m = DeclMapper(nullptr, nullptr).getModule(rootDecl);

    if (id == calypso.id__)
        m->searchInlineNamespaces();

    return m;
}

// ***** //

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

void Module::complete()
{
    Dsymbols* newMembers = new Dsymbols;
    newMembers->reserve(members->dim);

    DeclMapper mapper(this);

    auto addSymAndWrapper = [&](const clang::Decl* D)
    {
        if (auto sym = mapper.dsymForDecl(D))
            newMembers->push(sym);
        if (auto td = mapper.dsymForDecl<DeclMapper::WrapExplicitSpecsAndOverloadedOperators>(D))
            newMembers->push(td);
    };

    if (ident == calypso.id__)
    { // Hardcoded module with all the top-level non-tag decls + the anonymous tags of a namespace
        std::function<void(const clang::DeclContext *DC)> mapNamespace =
                [&] (const clang::DeclContext *DC)
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
                        mapNamespace(cast<clang::DeclContext>(*D));
                        continue;
                    }
                    else if (!isTopLevelInNamespaceModule(*D))
                        continue;

                    addSymAndWrapper(*D);
                }
            }
        };

        mapNamespace(cast<clang::DeclContext>(rootDecl));
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

            addSymAndWrapper(Decl);
        }

        // Add the non-member overloaded operators that are meant to work with this record/enum
        for (int I = 1; I < clang::NUM_OVERLOADED_OPERATORS; I++)
        {
            auto Op = static_cast<clang::OverloadedOperatorKind>(I);
            searchNonMemberOverloadedOperators(Op);

            for (auto OO: nonMemberOverloadedOperators[Op].OOs)
                addSymAndWrapper(OO);
        }
    }

    delete members;
    members = newMembers;
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
    if (!Friend)
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

clang::Expr* exprFromMacro(const clang::IdentifierInfo* II)
{
    auto& PP = calypso.getPreprocessor();
    auto& Context = calypso.getASTContext();
    auto& Sema = calypso.getSema();

    if (!PP.isMacroDefined(II))
        return nullptr;

    auto MInfo = PP.getMacroInfo(II);
    if (!MInfo)
        return nullptr;

    if (!MInfo->isObjectLike() || MInfo->isUsedForHeaderGuard() || MInfo->getNumTokens() > 1)
        return nullptr;

    auto MLoc = MInfo->getDefinitionLoc();

    if (MInfo->getNumTokens() == 0) {
        unsigned BoolSize = Context.getIntWidth(Context.BoolTy);
        return clang::IntegerLiteral::Create(Context, llvm::APInt(BoolSize, 1),
                                    Context.BoolTy, MLoc);
    } else {
        auto& Tok = MInfo->getReplacementToken(0);
        if (Tok.getKind() != clang::tok::numeric_constant)
            return nullptr;

        auto ResultExpr = Sema.ActOnNumericConstant(Tok);
        if (!ResultExpr.isInvalid())
            return ResultExpr.get(); // numeric_constant tokens might not be valid numerical expressions, e.g #define _SDT_ASM_ADDR .8byte
    }

    return nullptr;
}

Dsymbol* DeclMapper::dsymForMacro(Identifier* ident)
{
    auto TU = calypso.getASTContext().getTranslationUnitDecl();
    auto parent = getModule(TU);

    auto II = calypso.toIdentifierInfo(ident);

    // C allows macros and other symbols to share the same name, D doesn't
    // Conflicting macros must be prefixed with ℂ
    if (!TU->lookup(clang::DeclarationName(II)).empty())
        return nullptr;

    auto E = exprFromMacro(II);
    if (!E)
        return nullptr;

    ExprMapper expmap(*this);

    auto loc = fromLoc(E->getLocStart());
    auto e = expmap.fromExpression(E);
    auto ie = new_ExpInitializer(loc, e);

    auto v = new_VarDeclaration(loc, e->type, ident, ie);
    v->storage_class = STCmanifest;

    parent->members->push(v);
    v->addMember(nullptr, parent);

    return v;
}

Dsymbol *Module::search(const Loc& loc, Identifier *ident, int flags) // FIXME non member operators call mapDecls
{
    if (!(flags & MapOverloads))
        if (auto s = symtab->lookup(ident))
            return s;

    DeclMapper mapper(this);

    auto Name = calypso.toDeclarationName(ident);

    if (this->ident == calypso.id__)
    {
        auto DC = cast<clang::DeclContext>(rootDecl);

        for (auto Match: DC->lookup(Name))
            if (isTopLevelInNamespaceModule(Match))
                mapper.dsymAndWrapperForDecl(Match);

        for (auto NS: inlineNamespaces)
            for (auto Match: NS->lookup(Name))
                if (isTopLevelInNamespaceModule(Match))
                    mapper.dsymAndWrapperForDecl(Match);

        if (isa<clang::TranslationUnitDecl>(rootDecl)) // TODO: specific derived Module class for TU?
            mapper.dsymForMacro(ident);
    }
    else if (this->ident == ident)
    {
        mapper.dsymAndWrapperForDecl(rootDecl);
    }
    else
    {
        if (auto DC = dyn_cast<clang::DeclContext>(rootDecl))
            for (auto Match: DC->lookup(Name))
                if (isRecordMemberInModuleContext(Match))
                    mapper.dsymAndWrapperForDecl(Match);

        if (Name.getNameKind() == clang::DeclarationName::CXXOperatorName)
        {
            auto Op = Name.getCXXOverloadedOperator();
            searchNonMemberOverloadedOperators(Op);

            for (auto OO: nonMemberOverloadedOperators[Op].OOs)
                mapper.dsymAndWrapperForDecl(OO);
        }
    }

    return symtab->lookup(ident);
}

void Module::searchNonMemberOverloadedOperators(clang::OverloadedOperatorKind Op)
{
    if (nonMemberOverloadedOperators[Op].searched)
        return;
    nonMemberOverloadedOperators[Op].searched = true;

    if (!isa<clang::RecordDecl>(rootDecl) && !isa<clang::ClassTemplateDecl>(rootDecl))
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

// ===== //

const clang::Decl *getDecl(Dsymbol *s)
{
    assert(isCPP(s));

#define RETRIEVE(DECL, MEMBER) \
    if (s->is##DECL()) return static_cast<cpp::DECL*>(s)->MEMBER;

    RETRIEVE(StructDeclaration, RD)
    RETRIEVE(ClassDeclaration, RD)
    RETRIEVE(EnumDeclaration, ED)
    RETRIEVE(EnumMember, ECD)
    RETRIEVE(CtorDeclaration, CCD)
    RETRIEVE(DtorDeclaration, CDD)
    RETRIEVE(FuncDeclaration, FD)
    RETRIEVE(VarDeclaration, VD)

#undef RETRIEVE
    llvm_unreachable("Unhandled getDecl");
}

void markModuleForGenIfNeeded(Dsymbol *s)
{
    assert(isCPP(s));

    auto minst = s->getInstantiatingModule();
    assert(minst);
    if (!isCPP(minst))
        return;

    std::string MangledName;
    calypso.mangle(cast<clang::NamedDecl>(getDecl(s)), MangledName);

    auto c_minst = static_cast<cpp::Module*>(minst);
    if (!c_minst->emittedSymbols.count(MangledName)) {
        if (!c_minst->needGen) {
            c_minst->needGen = true;
            c_minst->emittedSymbols.clear(); // esp. important in case of separate compilation
        }
        c_minst->emittedSymbols.insert(MangledName);
    }
}

}
