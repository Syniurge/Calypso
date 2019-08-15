// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/cppimport.h"
#include "cpp/cpptemplate.h"
#include "cpp/ddmdstructor.h"
#include "cpp/ddmdvisitor.h"
#include "aggregate.h"
#include "expression.h"
#include "init.h"
#include "scope.h"

#include "clang/AST/Decl.h"
#include "clang/Sema/Sema.h"

int overloadApply(Dsymbol *fstart, void *param, int (*fp)(void *, Dsymbol *));

namespace cpp
{

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

void MarkModuleForGenIfNeeded(Dsymbol *s)
{
    assert(isCPP(s));
    calypso.mangle(s);

    const clang::Decl* D;
    if (auto fd = s->isFuncDeclaration())
        D = getFD(fd);
    else if (auto vd = s->isVarDeclaration())
        D = static_cast<cpp::VarDeclaration*>(vd)->VD;
    else
        llvm_unreachable("Unhandled symbol");

    auto& MangledName = calypso.MangledDeclNames[getCanonicalDecl(D)];
    auto minst = s->getInstantiatingModule();
    assert(minst && !MangledName.empty());

    if (isCPP(minst)) {
        auto c_minst = static_cast<cpp::Module*>(minst);
        if (!c_minst->emittedSymbols.count(MangledName)) {
            c_minst->needGen = true;
            c_minst->emittedSymbols.insert(MangledName);
        }
    }
}

VarDeclaration::VarDeclaration(Loc loc, Identifier *id,
                               const clang::ValueDecl *VD, Type *t, Initializer *init)
{
    construct_VarDeclaration(this, loc, t, id, init);
    this->VD = VD;
}

VarDeclaration::VarDeclaration(const VarDeclaration& o)
    : VarDeclaration(o.loc, o.ident, o.VD,
                     o.type ? o.type->syntaxCopy() : nullptr,
                     o._init ? o._init->syntaxCopy() : nullptr)
{
    storage_class = o.storage_class; // workaround for syntaxCopy because base method only assigns storage_class if the arg is null (BUG?)
}

bool VarDeclaration::isOverlappedWith(::VarDeclaration* v2)
{
    auto& Context = calypso.getASTContext();
    assert(isCPP(v2));
    auto c_v2 = static_cast<cpp::VarDeclaration*>(v2);
    auto Field1 = cast<clang::FieldDecl>(VD);
    auto Field2 = cast<clang::FieldDecl>(c_v2->VD);
    unsigned size1 = Field1->isBitField() ? Field1->getBitWidthValue(Context) : (type->size() * 8);
    unsigned size2 = Field2->isBitField() ? Field2->getBitWidthValue(Context) : (v2->type->size() * 8);
    return (offsetInBits < c_v2->offsetInBits + size2 &&
            c_v2->offsetInBits < offsetInBits + size1);
}

void MarkVarReferenced(::VarDeclaration* vd)
{
    if (vd->isEnumMember())
        return;

    auto c_vd = static_cast<cpp::VarDeclaration*>(vd);
    if (c_vd->isUsed)
        return;
    c_vd->isUsed = true;

    auto Var = dyn_cast<clang::VarDecl>(
                    const_cast<clang::ValueDecl*>(c_vd->VD));
    if (!Var)
        return;

    auto& S = calypso.getSema();

    if (Var->getTemplateSpecializationKind() == clang::TSK_ImplicitInstantiation)
        S.InstantiateVariableDefinition(Var->getLocation(), Var);

    if (vd->isDataseg())
        MarkModuleForGenIfNeeded(vd);
}

FuncDeclaration::FuncDeclaration(Loc loc, Identifier *id, StorageClass storage_class,
                    Type* type, const clang::FunctionDecl *FD)
{
    construct_FuncDeclaration(this, loc, loc, id, storage_class, type);
    this->FD = FD;
}

FuncDeclaration::FuncDeclaration(const FuncDeclaration& o)
    : FuncDeclaration(o.loc, o.ident, o.storage_class,
                      o.type ? o.type->syntaxCopy() : nullptr, o.FD)
{
}

::FuncDeclaration *FuncDeclaration::overloadCppMatch(::FuncDeclaration *fd,
                                                        const clang::FunctionDecl* FD)
{
    struct FDEquals
    {
        const clang::FunctionDecl* FD;            // type to match
        ::FuncDeclaration *f; // return value

        static int fp(void *param, Dsymbol *s)
        {
            if (!s->isFuncDeclaration() || !isCPP(s))
                return 0;
            auto f = static_cast<::FuncDeclaration*>(s);
            FDEquals *p = (FDEquals *)param;

            if (p->FD == getFD(f))
            {
                p->f = f;
                return 1;
            }

            return 0;
        }
    };
    FDEquals p;
    p.FD = FD;
    p.f = nullptr;
    overloadApply(fd, &p, &FDEquals::fp);
    return p.f;
}

CtorDeclaration::CtorDeclaration(Loc loc, StorageClass storage_class,
                                 Type* type, const clang::CXXConstructorDecl* CCD)
{
    construct_CtorDeclaration(this, loc, loc, storage_class, type);
    this->CCD = CCD;
}

CtorDeclaration::CtorDeclaration(const CtorDeclaration& o)
    : CtorDeclaration(o.loc, o.storage_class,
                      o.type ? o.type->syntaxCopy() : nullptr, o.CCD)
{
}

DtorDeclaration::DtorDeclaration(Loc loc, StorageClass storage_class,
                                 Identifier* id, const clang::CXXDestructorDecl* CDD)
{
    construct_DtorDeclaration(this, loc, loc, storage_class, id);
    this->CDD = CDD;
}

DtorDeclaration::DtorDeclaration(const DtorDeclaration& o)
    : DtorDeclaration(o.loc, o.storage_class, o.ident, o.CDD)
{
}

EnumDeclaration::EnumDeclaration(Loc loc, Identifier* id, Type* memtype,
                                 const clang::EnumDecl* ED)
{
    construct_EnumDeclaration(this, loc, id, memtype);
    this->ED = ED;
}

EnumDeclaration::EnumDeclaration(const EnumDeclaration &o)
    : EnumDeclaration(o.loc, o.ident,
                      o.memtype ? o.memtype->syntaxCopy() : nullptr, o.ED)
{
}

void EnumDeclaration::accept(Visitor *v)
{
    v->visit(this);

    if (v->_typeid() == TI_DsymbolSem1Visitor)
        if (!defaultval && !members)
            defaultval = defaultInit(memtype, loc); // C++ enums may be empty, and EnumDeclaration::getDefaultValue() errors if both defaultval and members are null
}

EnumMember::EnumMember(Loc loc, Identifier *id, Expression *value, Type *type,
               const clang::EnumConstantDecl *ECD)
{
    construct_EnumMember(this, loc, id, value, type);
    this->ECD = ECD;
}

Dsymbol* EnumMember::syntaxCopy(Dsymbol* s)
{
    if (!s)
        s = new cpp::EnumMember(loc, ident,
                      value() ? value()->syntaxCopy() : nullptr,
                      type ? type->syntaxCopy() : nullptr, ECD);
    assert(isCPP(s) && s->isEnumMember());
    static_cast<cpp::EnumMember*>(s)->ECD = ECD;
    return ::EnumMember::syntaxCopy(s);
}

void EnumMember::accept(Visitor *v)
{
    v->visit(this);
}

AliasDeclaration::AliasDeclaration(Loc loc, Identifier* ident,
                                Type* type, const clang::TypedefNameDecl* TND)
{
    construct_AliasDeclaration(this, loc, ident, type);
    this->TND = TND;
}

AliasDeclaration::AliasDeclaration(const AliasDeclaration &o)
    : AliasDeclaration(o.loc, o.ident, o.type->syntaxCopy(), o.TND)
{
    this->storage_class = o.storage_class;
}

Dsymbol* AliasDeclaration::syntaxCopy(Dsymbol* s)
{
    assert(!s);
    return new cpp::AliasDeclaration(*this); // hmm hmm
}

void AliasDeclaration::doSemantic()
{
    if (!_scope)
        return;

    isUsed = true;
    dsymbolSemantic(this, _scope);
}

Dsymbol *AliasDeclaration::toAlias()
{
    doSemantic();
    return ::AliasDeclaration::toAlias();
}

Dsymbol *AliasDeclaration::toAlias2()
{
    doSemantic();
    return ::AliasDeclaration::toAlias2();
}

void AliasDeclaration::accept(Visitor *v)
{
    if (v->_typeid() == TI_DsymbolSem1Visitor && !isUsed)
        ; // resolve aliases lazily, the DMD way of evaluating everything leads to infinite recursion for some C++ templates
          // ex.: typedef _Index_tuple<_Indexes, sizeof(_Indexes)> _Index_tuple<size_t... _Indexes>::__next;
    else
        v->visit(this);
}

IMPLEMENT_syntaxCopy(VarDeclaration, VD)
IMPLEMENT_syntaxCopy(FuncDeclaration, FD)
IMPLEMENT_syntaxCopy(CtorDeclaration, CCD)
IMPLEMENT_syntaxCopy(DtorDeclaration, CDD)
IMPLEMENT_syntaxCopy(EnumDeclaration, ED)

/***********************/

DeclReferencer::DeclReferencer(::Module* minst)
  : mapper(minst, false, false), expmap(mapper)
{
}

void DeclReferencer::Traverse(Loc loc, Scope *sc, clang::Stmt *S)
{
    this->loc = loc;
    this->sc = mapper.scSemImplicitImports = sc;

    // First map nested record decls (esp. lambda classes)
    // This needs to be done before because lambda CXXRecordDecl may be used before and/or after within the AST/
    NestedDeclMapper(*this).TraverseStmt(S);

    TraverseStmt(S);
}

bool DeclReferencer::Reference(const clang::NamedDecl *D)
{
    assert(!D->isInvalidDecl() && "Missed an invalid caller, fix Clang");

    for (const clang::Decl *DI = D; !isa<clang::TranslationUnitDecl>(DI); DI = cast<clang::Decl>(DI->getDeclContext()))
        if (auto RD = dyn_cast<clang::CXXRecordDecl>(DI))
            if (RD->isLocalClass())
                return true; // are local records emitted when emitting a function?

    if (auto VD = dyn_cast<clang::VarDecl>(D))
        if (!VD->isFileVarDecl())
            return true;

    if (auto FD = dyn_cast<clang::FunctionDecl>(D))
    {
        if (!isMapped(D))
            return true;
        if (FD->getBuiltinID())
            return true;
        if (FD->isExternC())
            return true; // FIXME: Clang 3.6 doesn't always map decls to the right source file,
                // so the path generated by typeQualifiedFor although correct will result in a failed lookup.
                // This may get fixed by 3.7.
    }

    if (D->d && D->d->sym) {
        calypso.markSymbolReferenced(D->d->sym);
        return true;
    }

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    if (D->isOutOfLine() && D->getFriendObjectKind() != clang::Decl::FOK_None)
    {
        auto DeclCtx = dyn_cast<clang::DeclContext>(D);
        if (DeclCtx && DeclCtx->isDependentContext()) // FIXME
            return true;
    }


//     auto Prim = Func ? Func->getPrimaryTemplate() : nullptr;
//     if (Prim)
//         D = cast<clang::NamedDecl>(getSpecializedDeclOrExplicit(Func));

    auto e = expmap.fromExpressionDeclRef(loc, /*Prim ? Prim :*/ const_cast<clang::NamedDecl*>(D),
                                            nullptr, TQ_OverOpFullIdent);
//     e = expressionSemantic(e, sc);

    Dsymbol *s;
    if (e->op == TOKvar)
        s = static_cast<SymbolExp*>(e)->var;
    else if (e->op == TOKtemplate)
        s = static_cast<TemplateExp*>(e)->td;
    else if (e->op == TOKtype)
        s = static_cast<TypeExp*>(e)->type->toDsymbol(nullptr);
    else {
        assert(e->op == TOKimport);
        s = static_cast<ScopeExp*>(e)->sds;
    }

    if (auto tempinst = s->isInstantiated())
        cppSemantic(tempinst, sc);
    else if (s->isAggregateDeclaration())
        cppSemantic(s, nullptr);

//     if (s->isFuncDeclaration() || s->isTemplateDeclaration())
//     {
//         struct DEquals
//         {
//             const clang::Decl* D;
//             Dsymbol *s = nullptr; // return value
//
//             static int fp(void *param, Dsymbol *s)
//             {
//                 DEquals *p = (DEquals *)param;
//
//                 if (!isCPP(s))
//                     return 0;
//
//                 auto fd = s->isFuncDeclaration();
//                 auto c_td = static_cast<cpp::TemplateDeclaration*>(
//                     s->isTemplateDeclaration());
//                 const clang::Decl* s_D = fd ? getFD(fd) : c_td->TempOrSpec;
//
//                 if (p->D == getCanonicalDecl(s_D)) {
//                     p->s = s;
//                     return 1;
//                 }
//
//                 return 0;
//             }
//         };
//         DEquals p;
//         p.D = getCanonicalDecl(D);
//         overloadApply(s, &p, &DEquals::fp);
//         s = p.s;
//         assert(s);
//     }

//     if (Func && Func->getPrimaryTemplate())
//     {
//         // if it's a template spec we must instantiate the right overload
//         assert(s->isTemplateDeclaration());
//
//         auto td = static_cast<cpp::TemplateDeclaration*>(s);
//         if (td->semanticRun == PASSinit) {
//             assert(td->_scope);
//             dsymbolSemantic(td, td->_scope); // this must be done here because havetempdecl being set to true it won't be done by findTempDecl()
//         }
//
//         auto tiargs = mapper.fromTemplateArguments(loc, Func->getTemplateSpecializationArgs());
//         assert(tiargs);
//
//         auto checkTiargs = [] (Objects* tiargs) {
//             for (auto& tiarg: *tiargs)
//                 if (!tiarg)
//                     return false;
//             return true;
//         };
//         if (!checkTiargs(tiargs))
//             goto Lcleanup;
//
//         SpecValue spec(mapper);
//         getIdentifier(Func, &spec, true);
//         if (spec)
//             tiargs->shift(spec.toTemplateArg(loc));
//
//         auto tempinst = new cpp::TemplateInstance(loc, td, tiargs);
//         tempinst->Inst = const_cast<clang::FunctionDecl*>(Func);
//         tempinst->semantictiargsdone = false; // NOTE: the "havetempdecl" ctor of Templateinstance set semantictiargsdone to true...
//         if (!tempinst->semanticTiargs(sc))
//             assert(false && "DeclReferencer semanticTiargs failed");
//
//         td->makeForeignInstance(tempinst, sc);
//         dsymbolSemantic(tempinst, sc);
//
//         s = tempinst->toAlias();
//     }
//     else
//     {
//         if (s->isAggregateDeclaration())
//             cppSemantic(s, nullptr);
//     }

    calypso.markSymbolReferenced(s);

// Lcleanup:
    // Memory usage can skyrocket when using a large library
    delete_Object(e);

    return true;
}

bool DeclReferencer::Reference(const clang::Type *T)
{
    if (auto TT = T->getAs<clang::TagType>())
        if (!TT->isUnionType())
            Reference(TT->getDecl());

    return true;
}

bool DeclReferencer::VisitCXXConstructExpr(const clang::CXXConstructExpr *E)
{
    auto ConstructedType = E->getType();
    if (!ConstructedType.isNull())
        Reference(ConstructedType.getTypePtr());
    return Reference(E->getConstructor());
}

bool DeclReferencer::VisitCXXNewExpr(const clang::CXXNewExpr *E)
{
    return Reference(E->getOperatorNew());
}

bool DeclReferencer::VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E)
{
    auto DestroyedType = E->getDestroyedType();
    if (!DestroyedType.isNull())
        Reference(DestroyedType.getTypePtr());

    return Reference(E->getOperatorDelete());
}

bool DeclReferencer::VisitDeclRef(const clang::NamedDecl *D)
{
    if (isa<clang::FunctionDecl>(D) || isa<clang::VarDecl>(D))
        return Reference(D);

    return true;
}

bool DeclReferencer::VisitDeclRefExpr(const clang::DeclRefExpr *E)
{
    return VisitDeclRef(E->getDecl());
}

bool DeclReferencer::VisitMemberExpr(const clang::MemberExpr *E)
{
    return VisitDeclRef(E->getMemberDecl());
}

/***********************/

bool NestedDeclMapper::VisitLambdaExpr(const clang::LambdaExpr *E)
{
    return TraverseCXXRecordDecl(E->getLambdaClass());
}

bool NestedDeclMapper::TraverseCXXRecordDecl(const clang::CXXRecordDecl *D)
{
    if (D->isLambda() && D->getLambdaManglingNumber() == 0)
        return true; // the lambda record is internal, no need to map it

    if (auto a = dref.mapper.VisitRecordDecl(D))
        for (auto s: *a)
            dsymbolSemantic(s, dref.sc);

    return true;
}

/***********************/

void InstantiateFunctionDefinition(clang::Sema &S, clang::FunctionDecl* D)
{
    auto& Diags = calypso.getDiagnostics();

    auto FPT = D->getType()->getAs<clang::FunctionProtoType>();
    if (FPT && clang::isUnresolvedExceptionSpec(FPT->getExceptionSpecType()))
        S.ResolveExceptionSpec(D->getLocation(), FPT);

    S.MarkFunctionReferenced(D->getLocation(), D);
    if (Diags.hasErrorOccurred())
        Diags.Reset();
    S.PerformPendingInstantiations();

    // MarkFunctionReferenced won't instantiate some implicitly instantiable functions
    // Not fully understanding why, but here's a second attempt
    if (!D->hasBody() && D->isImplicitlyInstantiable())
        S.InstantiateFunctionDefinition(D->getLocation(), D);
}

void InstantiateAndTraverseFunctionBody(::FuncDeclaration* fd, Scope *sc)
{
    auto& S = calypso.getSema();

    auto D = const_cast<clang::FunctionDecl*>(getFD(fd));
    assert(!D->getDeclContext()->isDependentContext());

    InstantiateFunctionDefinition(S, D);

    if (D->isInvalidDecl())
    {
        if (fd->parent->isTemplateInstance() && isCPP(fd->parent)) {
            auto c_ti = static_cast<cpp::TemplateInstance*>(fd->parent);
            c_ti->markInvalid();
        }
        return;
    }

    const clang::FunctionDecl *Def;
    if (D->hasBody(Def))
    {
        Scope *sc2 = sc->push();
        sc2->func = fd;
//         sc2->parent = fd;
        sc2->callSuper = 0;
        sc2->sbreak = NULL;
        sc2->scontinue = NULL;
        sc2->sw = NULL;
        sc2->stc &= ~(STCauto | STCscope | STCstatic | STCabstract |
                        STCdeprecated | STCoverride |
                        STC_TYPECTOR | STCfinal | STCtls | STCgshared | STCref | STCreturn |
                        STCproperty | STCnothrow | STCpure | STCsafe | STCtrusted | STCsystem);
        sc2->protection = {Prot::public_, nullptr};
        sc2->explicitProtection = 0;
        sc2->aligndecl = NULL;
        sc2->flags = sc->flags & ~SCOPEcontract;
        sc2->flags &= ~SCOPEcompile;
        sc2->tf = NULL;
        sc2->os = NULL;
        sc2->inLoop = 0;
        sc2->userAttribDecl = NULL;
        sc2->fieldinit = NULL;
        sc2->fieldinit_dim = 0;

        DeclReferencer declReferencer(sc->minst);
        declReferencer.Traverse(fd->loc, sc2, Def->getBody());

        if (auto Ctor = dyn_cast<clang::CXXConstructorDecl>(Def))
            for (auto& Init: Ctor->inits())
                declReferencer.Traverse(fd->loc, sc2, Init->getInit());
    }

    MarkModuleForGenIfNeeded(fd);
}

void MarkFunctionReferenced(::FuncDeclaration* fd)
{
    bool& isUsed = getIsUsed(fd);

    if (isUsed)
        return;
    isUsed = true;

    // Member *structor calls do not appear in the AST, hence DeclReferencer won't mark them
    // referenced and we have to do it here
    if (fd->isCtorDeclaration() || fd->isDtorDeclaration())
        fd->langPlugin()->markSymbolReferenced(fd->parent);

    if (fd->semanticRun >= PASSsemantic3done)
        InstantiateAndTraverseFunctionBody(fd, fd->_scope);
}

void FuncDeclaration::doSemantic3(::FuncDeclaration *fd)
{
    if (fd->semanticRun >= PASSsemantic3)
        return;
    fd->semanticRun = PASSsemantic3;
    fd->semantic3Errors = false;

    if (getIsUsed(fd)) {
        assert(fd->_scope);
        InstantiateAndTraverseFunctionBody(fd, fd->_scope);
    }

    fd->semanticRun = PASSsemantic3done;
}

void FuncDeclaration::accept(Visitor *v)
{
    auto v_ti = v->_typeid();

    if (v_ti == TI_DsymbolSem3Visitor) // semantic3
        doSemantic3(this);
    else
        v->visit(this);
}

void CtorDeclaration::accept(Visitor *v)
{
    auto v_ti = v->_typeid();

    if (v_ti == TI_DsymbolSem3Visitor) // semantic3
        cpp::FuncDeclaration::doSemantic3(this);
    else
        v->visit(this);
}

void DtorDeclaration::accept(Visitor *v)
{
    auto v_ti = v->_typeid();

    if (v_ti == TI_DsymbolSem3Visitor) // semantic3
        cpp::FuncDeclaration::doSemantic3(this);
    else
        v->visit(this);
}

const clang::FunctionDecl *getFD(::FuncDeclaration *f)
{
    assert(isCPP(f));

    if (f->isCtorDeclaration())
        return static_cast<CtorDeclaration*>(f)->CCD;
    else if (f->isDtorDeclaration())
        return static_cast<DtorDeclaration*>(f)->CDD;
    else
        return static_cast<FuncDeclaration*>(f)->FD;
}

const clang::Decl *getCanonicalDecl(const clang::Decl *D)
{
    D = D->getCanonicalDecl();

    if (D->getFriendObjectKind() != clang::Decl::FOK_None && D->isOutOfLine())
    {
        // if the canonical decl is an out-of-line friend decl and the actual decl is declared, prefer the latter to the former
        // to ensure that it ends up in the proper module, not the friend decl parent's module
        if (auto Func = dyn_cast<clang::FunctionDecl>(D)) {
            for (auto Redecl: Func->redecls())
                if (Redecl->getFriendObjectKind() == clang::Decl::FOK_None)
                    return Redecl;
        } else if (auto Tag = dyn_cast<clang::TagDecl>(D)) {
            for (auto Redecl: Tag->redecls())
                if (Redecl->getFriendObjectKind() == clang::Decl::FOK_None)
                    return Redecl;
        } else if (auto Temp = dyn_cast<clang::RedeclarableTemplateDecl>(D)) {
            for (auto Redecl: Temp->redecls())
                if (Redecl->getFriendObjectKind() == clang::Decl::FOK_None)
                    return Redecl;
        }
    }

    return D;
}

void MarkAggregateReferenced(::AggregateDeclaration* ad);

bool LangPlugin::isSymbolReferenced(Dsymbol *s)
{
    if (s->isStructDeclaration())
        return static_cast<cpp::StructDeclaration*>(s)->isUsed;
    else if (s->isClassDeclaration())
        return static_cast<cpp::ClassDeclaration*>(s)->isUsed;
    else if (auto fd = s->isFuncDeclaration())
        return getIsUsed(fd);
    else if (s->isVarDeclaration())
        return static_cast<cpp::VarDeclaration*>(s)->isUsed;
    return true; // FIXME
}

void LangPlugin::markSymbolReferenced(Dsymbol *s)
{
    if (auto fd = s->isFuncDeclaration())
        MarkFunctionReferenced(fd);
    else if (auto vd = s->isVarDeclaration())
        MarkVarReferenced(vd);
    else if (auto ad = s->isAggregateDeclaration())
        MarkAggregateReferenced(ad);
}

}
