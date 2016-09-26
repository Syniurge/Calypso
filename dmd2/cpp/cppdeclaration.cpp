// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/cpptemplate.h"
#include "aggregate.h"
#include "init.h"
#include "scope.h"

#include "clang/AST/Decl.h"

namespace cpp
{

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

DeclReferencer declReferencer;

VarDeclaration::VarDeclaration(Loc loc, Identifier *id,
                               const clang::ValueDecl *VD, Type *t, Initializer *init)
    : ::VarDeclaration(loc, t, id, init)
{
    this->VD = VD;
}

VarDeclaration::VarDeclaration(const VarDeclaration& o)
    : VarDeclaration(o.loc, o.ident, o.VD,
                     o.type ? o.type->syntaxCopy() : nullptr,
                     o.init ? o.init->syntaxCopy() : nullptr)
{
    storage_class = o.storage_class; // workaround for syntaxCopy because base method only assigns storage_class if the arg is null (BUG?)
}

bool VarDeclaration::overlap(::VarDeclaration* v2)
{
    return false; // HACK
}

FuncDeclaration::FuncDeclaration(Loc loc, Identifier *id, StorageClass storage_class,
                    Type* type, const clang::FunctionDecl *FD)
    : ::FuncDeclaration(loc, loc, id, storage_class, type)
{
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
    : ::CtorDeclaration(loc, loc, storage_class, type)
{
    this->CCD = CCD;
}

CtorDeclaration::CtorDeclaration(const CtorDeclaration& o)
    : CtorDeclaration(o.loc, o.storage_class,
                      o.type ? o.type->syntaxCopy() : nullptr, o.CCD)
{
}

DtorDeclaration::DtorDeclaration(Loc loc, StorageClass storage_class,
                                 Identifier* id, const clang::CXXDestructorDecl* CDD)
    : ::DtorDeclaration(loc, loc, storage_class, id)
{
    this->CDD = CDD;
}

DtorDeclaration::DtorDeclaration(const DtorDeclaration& o)
    : DtorDeclaration(o.loc, o.storage_class, o.ident, o.CDD)
{
}

EnumDeclaration::EnumDeclaration(Loc loc, Identifier* id, Type* memtype,
                                 const clang::EnumDecl* ED)
    : ::EnumDeclaration(loc, id, memtype)
{
    this->ED = ED;
}

EnumDeclaration::EnumDeclaration(const EnumDeclaration &o)
    : EnumDeclaration(o.loc, o.ident,
                      o.memtype ? o.memtype->syntaxCopy() : nullptr, o.ED)
{
}

AliasDeclaration::AliasDeclaration(Loc loc, Identifier* ident,
                                Type* type, const clang::TypedefNameDecl* TND)
    : ::AliasDeclaration(loc, ident, type)
{
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

// Resolve aliases lazily, the DMD way of evaluating everything leads to infinite recursion for some C++ templates
// Ex.: typedef _Index_tuple<_Indexes, sizeof(_Indexes)> _Index_tuple<size_t... _Indexes>::__next;
void AliasDeclaration::semantic(Scope *sc)
{
}

void AliasDeclaration::doSemantic()
{
    if (!scope)
        return;

    ::AliasDeclaration::semantic(scope);
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

IMPLEMENT_syntaxCopy(VarDeclaration, VD)
IMPLEMENT_syntaxCopy(FuncDeclaration, FD)
IMPLEMENT_syntaxCopy(CtorDeclaration, CCD)
IMPLEMENT_syntaxCopy(DtorDeclaration, CDD)
IMPLEMENT_syntaxCopy(EnumDeclaration, ED)

bool FuncDeclaration::cppSemantic(::FuncDeclaration *fd, Scope *sc)
{
    if (fd->semanticRun >= PASSsemanticdone)
        return true;

    auto FD = getFD(fd);

    if (!FD)
    {
        assert(fd->storage_class & STCdisable); // e.g added @disable this for structs without a C++ default ctor
        return true;
    }

    if (FD->getDescribedFunctionTemplate())
    {
        auto ti = sc->parent->isTemplateInstance();
        assert(ti && isCPP(ti->inst));
        auto c_ti = static_cast<cpp::TemplateInstance*>(ti->inst);

        auto Inst = cast<clang::FunctionDecl>(
                    c_ti->Inst.get<clang::NamedDecl*>());

        assert(isCPP(sc->module));
        DeclMapper m(static_cast<cpp::Module*>(sc->module));
        m.addImplicitDecls = false;

        if (auto inst = m.VisitInstancedFunctionTemplate(Inst))
            inst->syntaxCopy(fd);
        else
        {
            assert(fd->parent->isTemplateInstance());
            auto ti = static_cast<::TemplateInstance*>(fd->parent);
            fd->errors = ti->errors = true;
            return false;
        }
    }

    return true;
}

void FuncDeclaration::semantic(Scope *sc)
{
    if (!FuncDeclaration::cppSemantic(this, sc))
        return;
    ::FuncDeclaration::semantic(sc);
}

void CtorDeclaration::semantic(Scope *sc)
{
    if (!cpp::FuncDeclaration::cppSemantic(this, sc))
        return;
    ::CtorDeclaration::semantic(sc);
}

void DtorDeclaration::semantic(Scope *sc)
{
    if (!cpp::FuncDeclaration::cppSemantic(this, sc))
        return;
    ::DtorDeclaration::semantic(sc);
}

void DeclReferencer::Traverse(Loc loc, Scope *sc, clang::Stmt *S)
{
    this->loc = loc;
    this->sc = sc;
    TraverseStmt(S);
}

bool DeclReferencer::Reference(const clang::NamedDecl *D)
{
    if (D->isInvalidDecl())
        return true;

    for (const clang::Decl *DI = D; !isa<clang::TranslationUnitDecl>(DI); DI = cast<clang::Decl>(DI->getDeclContext()))
        if (auto RD = dyn_cast<clang::CXXRecordDecl>(DI))
            if (RD->isLocalClass())
                return true; // are local records emitted when emitting a function? if no this is a FIXME

    if (auto VD = dyn_cast<clang::VarDecl>(D))
        if (!VD->isFileVarDecl())
            return true;

    if (auto FD = dyn_cast<clang::FunctionDecl>(D))
    {
        if (!isMapped(D))
            return true;
        if (FD->getBuiltinID() ||
                (FD->isOverloadedOperator() && FD->isImplicit()))
            return true;
        if (FD->isExternC())
            return true; // FIXME: Clang 3.6 doesn't always map decls to the right source file,
                // so the path generated by typeQualifiedFor although correct will result in a failed lookup.
                // This may get fixed by 3.7.
    }

    if (Referenced.count(D->getCanonicalDecl()))
        return true;
    Referenced.insert(D->getCanonicalDecl());

    // Although we try to add all the needed imports during importAll(), sometimes we miss a module so ensure it gets loaded
    auto im = mapper.AddImplicitImportForDecl(loc, D, true);
    im->isstatic = true;
    auto dst = Package::resolve(im->packages, NULL, &im->pkg);
    if (!dst->lookup(im->id))
    {
        im->semantic(sc);
        im->semantic2(sc);
    }

    ReferenceTemplateArguments(D);

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    if (Func && Func->getPrimaryTemplate())
        D = cast<clang::NamedDecl>(getCanonicalDecl(getSpecializedDeclOrExplicit(Func)));

    // HACK FIXME
    if (Func && Func->isOutOfLine() &&
            Func->getFriendObjectKind() != clang::Decl::FOK_None)
    {
        auto Pattern = Func;
        if (auto MemberFunc = Func->getInstantiatedFromMemberFunction())
            Pattern = MemberFunc;

        if (Pattern->isDependentContext())
            return true;
    }

    auto e = expmap.fromExpressionDeclRef(loc, const_cast<clang::NamedDecl*>(D),
                                            nullptr, TQ_OverOpSkipSpecArg);
    e = e->semantic(sc);

    if (Func && Func->getPrimaryTemplate())
    {
        assert(e->op == TOKvar || e->op == TOKtemplate || e->op == TOKimport);
        Dsymbol *s;
        if (e->op == TOKvar)
            s = static_cast<SymbolExp*>(e)->var;
        else if (e->op == TOKtemplate)
            s = static_cast<TemplateExp*>(e)->td;
        else
            s = static_cast<ScopeExp*>(e)->sds;

        // if it's a non-template function there's nothing to do, it will be semantic'd along with its declcontext
        // if it's a template spec we must instantiate the right overload
        struct DEquals
        {
            const clang::Decl* D;
            Dsymbol *s = nullptr; // return value

            static int fp(void *param, Dsymbol *s)
            {
                if (!isCPP(s))
                    return 0;
                auto fd = s->isFuncDeclaration();
                auto td = static_cast<cpp::TemplateDeclaration*>(
                                            s->isTemplateDeclaration());
                DEquals *p = (DEquals *)param;

                decltype(D) s_D = fd ? getFD(fd) : td->TempOrSpec;

                if (p->D == getCanonicalDecl(s_D))
                {
                    p->s = s;
                    return 1;
                }

                return 0;
            }
        };
        DEquals p;
        p.D = D;
        overloadApply(s, &p, &DEquals::fp);
        assert(p.s && p.s->isTemplateDeclaration());

        auto td = static_cast<cpp::TemplateDeclaration*>(p.s->isTemplateDeclaration());
        if (td->semanticRun == PASSinit)
        {
            assert(td->scope);
            td->semantic(td->scope); // this must be done here because havetempdecl being set to true it won't be done by findTempDecl()
            assert(td->semanticRun > PASSinit);
        }

        auto tiargs = mapper.fromTemplateArguments(loc, Func->getTemplateSpecializationArgs());
        assert(tiargs);

        auto checkTiargs = [] (Objects* tiargs) {
            for (auto& tiarg: *tiargs)
                if (!tiarg)
                    return false;
            return true;
        };
        if (!checkTiargs(tiargs)) // TODO: C++11 code may use lambdas, should we reference them as well?
            goto Lcleanup;

        SpecValue spec(mapper);
        getIdentifier(Func, &spec, true);
        if (spec)
            tiargs->shift(spec.toTemplateArg(loc));
        
        auto tempinst = new cpp::TemplateInstance(loc, td, tiargs);
        tempinst->Inst = const_cast<clang::FunctionDecl*>(Func);
        tempinst->semantictiargsdone = false; // NOTE: the "havetempdecl" ctor of Templateinstance set semantictiargsdone to true...
        if (!tempinst->semanticTiargs(sc))
            assert(false && "DeclReferencer semanticTiargs failed");

        td->makeForeignInstance(tempinst);
        tempinst->semantic(sc);
    }

Lcleanup:
    // Memory usage can skyrocket when using a large library
    delete e;

    return true;
}

bool DeclReferencer::Reference(const clang::Type *T)
{
    if (auto TT = T->getAs<clang::TagType>())
        if (!TT->isUnionType())
            Reference(TT->getDecl());

    return true;
}

void DeclReferencer::ReferenceTemplateArguments(const clang::NamedDecl *D)
{
    const clang::TemplateArgumentList *InstArgs = nullptr;

    if (auto Func = dyn_cast<clang::FunctionDecl>(D))
        InstArgs = Func->getTemplateSpecializationArgs();

    if (!InstArgs)
        return;

    for (auto& Arg: InstArgs->asArray())
    {
        switch (Arg.getKind())
        {
            case clang::TemplateArgument::Expression:
                TraverseStmt(Arg.getAsExpr());
                break;
            case clang::TemplateArgument::Type:
                Reference(Arg.getAsType().getTypePtr());
                break;
            case clang::TemplateArgument::Template:
                // TODO
                break;
            default:
                break;
        }
    }
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

void FuncDeclaration::semantic3reference(::FuncDeclaration *fd, Scope *sc)
{
    if (fd->semanticRun >= PASSsemantic3)
        return;
    fd->semanticRun = PASSsemantic3;
    fd->semantic3Errors = false;

    auto FD = getFD(fd);
    if (!FD)
        return;

    const clang::FunctionDecl *Def;
    if (!FD->isInvalidDecl() && FD->hasBody(Def))
    {
        declReferencer.Traverse(fd->loc, sc, Def->getBody());

        if (auto Ctor = dyn_cast<clang::CXXConstructorDecl>(FD))
            for (auto& Init: Ctor->inits())
                declReferencer.Traverse(fd->loc, sc, Init->getInit());
    }

    fd->semanticRun = PASSsemantic3done;
}

void FuncDeclaration::semantic3(Scope *sc)
{
    semantic3reference(this, sc);
}

void CtorDeclaration::semantic3(Scope *sc)
{
    cpp::FuncDeclaration::semantic3reference(this, sc);
}

void DtorDeclaration::semantic3(Scope *sc)
{
    cpp::FuncDeclaration::semantic3reference(this, sc);
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

    if (D->getFriendObjectKind() != clang::Decl::FOK_None && D->isOutOfLine() &&
            isa<clang::FunctionDecl>(D))
    {
        auto FD = cast<clang::FunctionDecl>(D);

        // if the canonical decl is an out-of-ilne friend' decl and the actual decl is declared, prefer the latter to the former
        // to ensure that it ends up in the proper module, not the friend decl parent's module
        for (auto Redecl: FD->redecls())
            if (Redecl->getFriendObjectKind() == clang::Decl::FOK_None)
                return Redecl;
    }

    return D;
}

}
