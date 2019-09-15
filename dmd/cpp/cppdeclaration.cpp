// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
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
    assert(isCPP(v2));

    auto& Context = calypso.getASTContext();
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
        markModuleForGenIfNeeded(vd);
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

    this->type = new_TypeFunction(nullptr, Type::tvoid, false, LINKcpp, this->storage_class);
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

void EnumDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    Dsymbol::addMember(sc, sds);
}

Dsymbol *EnumDeclaration::search(const Loc &loc, Identifier *ident, int flags)
{
    if (auto s = ScopeDsymbol::search(loc, ident, flags))
        return s;

    auto Name = calypso.toDeclarationName(ident);
    for (auto Match: ED->lookup(Name))
        dsymForDecl(this, Match);

    return ScopeDsymbol::search(loc, ident, flags);
}

void EnumDeclaration::complete()
{
    Dsymbols* newMembers = new Dsymbols;
    newMembers->reserve(members->dim);

    for (auto ECD: ED->enumerators())
        newMembers->push(dsymForDecl(this, ECD));

    delete members;
    members = newMembers;
}

Expression *EnumDeclaration::getDefaultValue(const Loc &loc)
{
    if (defaultval)
        return defaultval;

    auto I = ED->enumerator_begin();

    if (I == ED->enumerator_end())
        // no members or forward declaration (e.g std::regex_constants::syntax_option_type)
        defaultval = memtype->defaultInitLiteral(loc);
    else
    {
        auto em = static_cast<EnumMember*>(dsymForDecl(this, *I));
        defaultval = em->value();
    }

    return defaultval;
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

void EnumMember::addMember(Scope *sc, ScopeDsymbol *sds)
{
    if (sds->isAnonymous()) {
        assert(sds->parent->isScopeDsymbol());
        sds = static_cast<ScopeDsymbol*>(sds->parent);
    }
    Dsymbol::addMember(sc, sds);
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

Type *AliasDeclaration::getType()
{
    if (!type)
        type = DeclMapper(this).fromType(TND->getUnderlyingType(), loc);
    return type;
}

Dsymbol *AliasDeclaration::toAlias()
{
    if (!aliassym)
        aliassym = getType()->toDsymbol(nullptr);
    return aliassym ? aliassym->toAlias() : this;
}

Dsymbol *AliasDeclaration::toAlias2()
{
    return toAlias();
}

IMPLEMENT_syntaxCopy(VarDeclaration, VD)
IMPLEMENT_syntaxCopy(FuncDeclaration, FD)
IMPLEMENT_syntaxCopy(CtorDeclaration, CCD)
IMPLEMENT_syntaxCopy(DtorDeclaration, CDD)
IMPLEMENT_syntaxCopy(EnumDeclaration, ED)

/***********************/

DeclReferencer::DeclReferencer(::FuncDeclaration* fdinst)
  : fdinst(fdinst)
{
}

void DeclReferencer::Traverse(const clang::FunctionDecl* D)
{
    const clang::FunctionDecl *Def;
    if (D->hasBody(Def))
    {
        TraverseStmt(Def->getBody());

        if (auto Ctor = dyn_cast<clang::CXXConstructorDecl>(Def))
            for (auto& Init: Ctor->inits())
                TraverseStmt(Init->getInit());
    }
}

bool isTemplateInstantiation(const clang::NamedDecl* D)
{
    if (auto Func = dyn_cast<clang::FunctionDecl>(D))
        return Func->isTemplateInstantiation();
    if (auto Var = dyn_cast<clang::VarDecl>(D))
        return clang::isTemplateInstantiation(Var->getTemplateSpecializationKind());
    return false;
}

bool DeclReferencer::Reference(const clang::NamedDecl *D)
{
    assert(!D->isInvalidDecl() && "Missed an invalid caller, fix Clang");

    D = cast<clang::NamedDecl>(getCanonicalDecl(D));

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    auto Var = dyn_cast<clang::VarDecl>(D);

    if (Var && !Var->isFileVarDecl())
        return true;

    if (Func && Func->getBuiltinID())
        return true;

    if (!D->d)
        const_cast<clang::NamedDecl*>(D)->d = new cpp::DData;

    if (auto sym = D->d->sym)
        calypso.markSymbolReferenced(sym);
    else if (!D->d->instantiatedBy)
    {
        if (isTemplateInstantiation(D) && !isCPP(fdinst->getInstantiatingModule()))
        {
            D->d->instantiatedBy = fdinst;
            instantiatedDecls(fdinst).insert(D);
        }
        else
        {
            auto Parent = GetNonNestedContext(D);
            auto parentMod = DeclMapper(fdinst).getModule(Parent);

            D->d->instantiatedBy = parentMod;
            parentMod->instantiatedDecls.insert(D);
        }

        if (Func)
            Traverse(Func);
        else if (auto Record = dyn_cast<clang::CXXRecordDecl>(D))
        {
            auto& Context = calypso.getASTContext();

            auto Key = Context.getCurrentKeyFunction(Record);
            const clang::FunctionDecl* Body;
            if (!Key || (Key->hasBody(Body) && Context.DeclMustBeEmitted(Body)))
            {
                for (auto MD: Record->methods())
                    if (MD->isVirtual())
                        Reference(MD);
            }
        }
    }

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

void InstantiateFunctionDefinition(clang::Sema &S, clang::FunctionDecl* D)
{
    auto& Diags = calypso.getDiagnostics();

    auto FPT = D->getType()->getAs<clang::FunctionProtoType>();
    if (FPT && clang::isUnresolvedExceptionSpec(FPT->getExceptionSpecType()))
        S.ResolveExceptionSpec(D->getLocation(), FPT);

    assert(S.getCurScope());

    S.MarkFunctionReferenced(D->getLocation(), D);
    if (Diags.hasErrorOccurred())
        Diags.Reset();
    S.PerformPendingInstantiations();

    // MarkFunctionReferenced won't instantiate some implicitly instantiable functions
    // Not fully understanding why, but here's a second attempt
    if (!D->hasBody() && D->isImplicitlyInstantiable())
        S.InstantiateFunctionDefinition(D->getLocation(), D);
}

void InstantiateAndTraverseFunctionBody(::FuncDeclaration* fd)
{
    auto& S = calypso.getSema();

    auto D = const_cast<clang::FunctionDecl*>(getFD(fd));
    assert(!D->getDeclContext()->isDependentContext());

    InstantiateFunctionDefinition(S, D);

    if (D->isInvalidDecl())
    {
        if (fd->parent->isTemplateInstance() && isCPP(fd->parent))
        {
            auto c_ti = static_cast<cpp::TemplateInstance*>(fd->parent);
            c_ti->markInvalid();
        }
        return;
    }

    DeclReferencer declReferencer(fd);
    declReferencer.Traverse(D);
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

    InstantiateAndTraverseFunctionBody(fd);

    markModuleForGenIfNeeded(fd);
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

InstantiatedDeclSet& instantiatedDecls(Dsymbol* instantiatedBy)
{
    if (instantiatedBy->isCtorDeclaration())
        return static_cast<CtorDeclaration*>(instantiatedBy)->instantiatedDecls;
    else if (instantiatedBy->isDtorDeclaration())
        return static_cast<DtorDeclaration*>(instantiatedBy)->instantiatedDecls;
    else if (instantiatedBy->isFuncDeclaration())
        return static_cast<FuncDeclaration*>(instantiatedBy)->instantiatedDecls;
    else
    {
        assert(instantiatedBy->isModule());
        return static_cast<Module*>(instantiatedBy)->instantiatedDecls;
    }
}

}
