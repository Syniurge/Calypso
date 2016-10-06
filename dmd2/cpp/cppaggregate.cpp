// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cpptemplate.h"
#include "attrib.h"
#include "scope.h"
#include "target.h"
#include "template.h"
#include "identifier.h"
#include "id.h"

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/VTableBuilder.h"
#include "clang/Sema/Sema.h"

namespace cpp
{

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

template<typename AggTy> void buildAggLayout(AggTy *ad);

StructDeclaration::StructDeclaration(Loc loc, Identifier* id,
                                     const clang::RecordDecl* RD)
    : ::StructDeclaration(loc, id)
{
    this->RD = RD;
}

StructDeclaration::StructDeclaration(const StructDeclaration& o)
    : StructDeclaration(o.loc, o.ident, o.RD)
{
}

ClassDeclaration::ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses,
                                   const clang::CXXRecordDecl *RD)
    : ::ClassDeclaration(loc, id, baseclasses)
{
    this->RD = RD;
}

ClassDeclaration::ClassDeclaration(const ClassDeclaration& o)
    : ClassDeclaration(o.loc, o.ident, nullptr, o.RD)
{ // NOTE: baseclasses will be duplicated by syntaxCopy, but this makes the copy constructor not doing what it should be doing
}

UnionDeclaration::UnionDeclaration(Loc loc, Identifier* id,
                                     const clang::RecordDecl* RD)
    : ::UnionDeclaration(loc, id)
{
    this->RD = RD;
}

UnionDeclaration::UnionDeclaration(const UnionDeclaration& o)
    : UnionDeclaration(o.loc, o.ident, o.RD)
{
}

IMPLEMENT_syntaxCopy(StructDeclaration, RD)
IMPLEMENT_syntaxCopy(ClassDeclaration, RD)
IMPLEMENT_syntaxCopy(UnionDeclaration, RD)


void StructDeclaration::semantic(Scope *sc)
{
    ::StructDeclaration::semantic(sc);
    const_cast<clang::RecordDecl*>(RD)->dsym = this;
}

Expression *StructDeclaration::defaultInit(Loc loc)
{
    if (!defaultCtor)
        return ::StructDeclaration::defaultInit(loc);

    auto arguments = new Expressions;
    return new CallExp(loc, new TypeExp(loc, type), arguments);
}

bool StructDeclaration::mayBeAnonymous()
{
    return true;
}

void StructDeclaration::buildLayout()
{
    buildAggLayout(this);
}

void StructDeclaration::finalizeSize(Scope* sc)
{
    ::StructDeclaration::finalizeSize(sc);
    if (defaultCtor)
        zeroInit = 0;
}

void ClassDeclaration::semantic(Scope *sc)
{
//     if (semanticRun >= PASSsemanticdone)
//         return;

    ::ClassDeclaration::semantic(sc);
    const_cast<clang::CXXRecordDecl*>(RD)->dsym = this;

//     // Build a copy ctor alias after scope setting and semantic'ing the C++ copy ctor during which its type is adjusted
//     if (semanticRun >= PASSsemanticdone)
//         buildCpCtor(sc);
}

void ClassDeclaration::buildCpCtor(Scope *sc)
{
//     auto& S = calypso.getSema();
//     auto _RD = const_cast<clang::CXXRecordDecl*>(RD);
//
//     auto CD = S.LookupCopyingConstructor(_RD, clang::Qualifiers::Const);
//     if (!CD)
//         CD = S.LookupCopyingConstructor(_RD, 0);
//
//     if (!CD)
//         return;
//
//     auto cpctor = findMethod(this, CD);
//     if (!cpctor)
//         return; // could be deleted or invalid
//
//     auto fwdcpctor = new OverloadAliasDeclaration(loc, Identifier::idPool("__cpctor"),
//                                     new TypeIdentifier(loc, Id::ctor), static_cast<TypeFunction*>(cpctor->type));
//     members->push(fwdcpctor);
//
//     fwdcpctor->addMember(sc, this, 1);
//     fwdcpctor->setScope(cpctor->scope);
//     fwdcpctor->semantic(cpctor->scope);
}

bool ClassDeclaration::mayBeAnonymous()
{
    return true;
}

template <typename AggTy>
 bool isBaseOfImpl(AggTy* base, ::ClassDeclaration* cd, int* poffset)
{
    auto RD = cast<clang::CXXRecordDecl>(getRecordDecl(base));

    if (!base->isBaseOf2(cd))
        return false;

    if (poffset)
    {
        *poffset = 0;

        // If the derived class is a D one, class instances need to hold two __vptr pointers
        // Their header will be:
        //      D __vptr
        //      D __monitor
        //      C++ __vptr
        if (!isCPP(cd))
            *poffset = Target::ptrsize * 2;

        while (!isCPP(cd))
            cd = static_cast<::ClassDeclaration*>(cd->baseClass);

        auto& Context = calypso.getASTContext();
        auto RD2 = static_cast<cpp::ClassDeclaration*>(cd)->RD;

        if (RD->getCanonicalDecl() == RD2->getCanonicalDecl())
            return true;

        auto Offset = clang::ComputeBaseOffset(Context, RD, RD2);
        *poffset += Offset.NonVirtualOffset.getQuantity();
    }

    return true;
}

bool StructDeclaration::isBaseOf(::ClassDeclaration* cd, int *poffset)
{
    return isBaseOfImpl(this, cd, poffset);
}

bool ClassDeclaration::isBaseOf(::ClassDeclaration *cd, int *poffset)
{
    return isBaseOfImpl(this, cd, poffset);
}

void ClassDeclaration::interfaceSemantic(Scope *sc)
{
}

Expression *ClassDeclaration::defaultInit(Loc loc)
{
    if (!defaultCtor)
        return ::ClassDeclaration::defaultInit(loc);

    auto arguments = new Expressions;
    return new CallExp(loc, new TypeExp(loc, type), arguments);
}

void ClassDeclaration::makeNested()
{
    // do not add vthis
}

// NOTE: the "D" vtbl isn't used unless a D class inherits from a C++ one
// Note that Func::semantic will re-set methods redundantly (although it's useful as a sanity check and it also sets vtblIndex),
// but vanilla doesn't know how to deal with multiple inheritance hence the need to query Clang.

// Why is this needed? Because D vtbls are only built after the first base class, so this is actually the cleanest and easiest way
// to take C++ multiple inheritance into account. No change to FuncDeclaration::semantic needed.
void ClassDeclaration::finalizeVtbl()
{
    clang::CXXFinalOverriderMap FinaOverriders;
    RD->getFinalOverriders(FinaOverriders);

    llvm::DenseSet<const clang::CXXMethodDecl*> inVtbl;

    for (auto I = FinaOverriders.begin(), E = FinaOverriders.end();
         I != E; ++I)
    {
        auto OverMD = I->second.begin()->second.front().Method;
        if (inVtbl.count(OverMD))
            continue;

        auto md = findMethod(this, OverMD);
        if (!md)
            continue;

        inVtbl.insert(OverMD);

        auto vi = md->findVtblIndex(&vtbl, vtbl.dim);
        if (vi < 0)
            vtbl.push(md);
    }
}

void ClassDeclaration::buildLayout()
{
    buildAggLayout(this);
}

bool UnionDeclaration::mayBeAnonymous()
{
    return true;
}

void UnionDeclaration::buildLayout()
{
    buildAggLayout(this);
}

AnonDeclaration::AnonDeclaration(Loc loc, bool isunion, Dsymbols* decl)
    : ::AnonDeclaration(loc, isunion, decl)
{
}

Dsymbol* AnonDeclaration::syntaxCopy(Dsymbol* s)
{
    assert(!s);
    auto a = new AnonDeclaration(loc, isunion, decl);
    a->AnonField = AnonField;
    return a;
}

// NOTE: we need to adjust every "this" pointer when accessing fields from bases
// This is what Clang does in Sema::PerformObjectMemberConversion
Expression *LangPlugin::getRightThis(Loc loc, Scope *sc, ::AggregateDeclaration *ad,
        Expression *e1, Declaration *var, int)
{
    Type *t = e1->type->toBasetype();
    AggregateDeclaration* tad;
    if (t->ty == Tpointer)
        tad = getAggregateSym(t->nextOf());
    else
        tad = getAggregateSym(t);

    if (ad == tad)
        return e1;

    ::ClassDeclaration *tcd = t->isClassHandle();
    assert(tcd && ad->isBaseOf2(tcd));

    e1 = new CastExp(loc, e1, ad->getType()); // NOTE: not strictly necessary if first base
    e1 = e1->semantic(sc);

    return e1;
}

Expression *LangPlugin::callCpCtor(Scope *sc, Expression *e)
{
    // TODO
    return nullptr;
}

::FuncDeclaration *LangPlugin::buildDtor(::AggregateDeclaration *ad, Scope *sc)
{
    assert(ad->dtors.dim < 2);

    if (ad->dtors.empty())
        return nullptr; // forward reference

    return ad->dtors[0];
}

::FuncDeclaration *LangPlugin::buildOpAssign(::StructDeclaration *sd, Scope *sc)
{
    if (auto f = hasIdentityOpAssign(sd, sc))
    {
        sd->hasIdentityAssign = true;
        return f;
    }

    return nullptr; // do not build an opAssign if none was mapped
}

template <typename AggTy>
 void buildAggLayout(AggTy *ad)
{
    assert(isCPP(ad));

    if (ad->layoutQueried)
        return;

    if (ad->RD->isInvalidDecl() || !ad->RD->getDefinition())
    {
       // if it's a forward reference, consider the record empty
        ad->structsize = 1;
        ad->alignsize = 1;
        return;
    }

    auto& Context = calypso.getASTContext();
    auto& RL = Context.getASTRecordLayout(ad->RD);

    ad->alignment = ad->alignsize = RL.getAlignment().getQuantity();
    ad->structsize = RL.getSize().getQuantity();

    std::function<void(Dsymbols *, unsigned, const clang::ASTRecordLayout&)>
        addRecord = [&] (Dsymbols *members, unsigned baseoffset,  const clang::ASTRecordLayout& Layout)
    {
        for (auto m: *members)
        {
            if (auto vd = m->isVarDeclaration())
            {
                assert(isCPP(vd));

                auto c_vd = static_cast<VarDeclaration*>(vd);
                auto FD = dyn_cast<clang::FieldDecl>(c_vd->VD);

                if (!FD)
                    continue;

                auto fldIdx = FD->getFieldIndex();
                vd->offset = baseoffset + Layout.getFieldOffset(fldIdx) / 8;

                ad->fields.push(vd);
            }
            else if (auto anon = m->isAttribDeclaration())
            {
                assert(/*anon->isAnonDeclaration() && */isCPP(anon));
                auto AnonField = static_cast<cpp::AnonDeclaration*>(anon)->AnonField;
                auto AnonRecord = AnonField->getType()->castAs<clang::RecordType>()->getDecl();

                auto AnonFieldIdx = AnonField->getFieldIndex();
                auto& AnonLayout = Context.getASTRecordLayout(AnonRecord);

                addRecord(anon->decl, baseoffset + Layout.getFieldOffset(AnonFieldIdx) / 8, AnonLayout);
            }
        }
    };

    addRecord(ad->members, 0, RL);

    ad->layoutQueried = true;
}

const clang::RecordDecl *getRecordDecl(::AggregateDeclaration *ad)
{
    assert(isCPP(ad));

    if (auto sd = ad->isStructDeclaration())
        return static_cast<StructDeclaration*>(sd)->RD;
    else if (auto cd = ad->isClassDeclaration())
        return static_cast<ClassDeclaration*>(cd)->RD;

   llvm_unreachable("Unknown aggregate decl type?");
}

const clang::RecordDecl *getRecordDecl(::Type *t)
{
    ::AggregateDeclaration *ad;

    switch(t->ty)
    {
        case Tstruct:
            ad = static_cast<TypeStruct*>(t)->sym;
            break;
        case Tclass:
            ad = static_cast<TypeClass*>(t)->sym;
            break;
        default:
            llvm_unreachable("Non-aggregate type");
    }

    return getRecordDecl(ad);
}

::FuncDeclaration *findMethod(::AggregateDeclaration *ad, const clang::FunctionDecl* FD)
{
    TypeMapper tmap;
    tmap.addImplicitDecls = false;

    auto ident = getExtendedIdentifier(FD, tmap);

    auto s = ad->ScopeDsymbol::search(ad->loc, ident);
    if (s && s->isFuncDeclaration())
    {
        assert(isCPP(s));
        auto fd = static_cast<::FuncDeclaration*>(s);
        fd = FuncDeclaration::overloadCppMatch(fd, FD);
        if (fd)
            return fd;
    }

    // search in base classes
    if (auto cd = ad->isClassDeclaration())
        for (auto *b: *cd->baseclasses)
        {
            if (!isCPP(b->base)) // skip Object
                continue;

            auto result = findMethod(b->base, FD);
            if (result)
                return result;
        }

    return nullptr;
}

::FuncDeclaration* findOverriddenMethod(::FuncDeclaration *md, ::ClassDeclaration *base)
{
    for (auto s2: base->vtbl)
    {
        auto md2 = s2->isFuncDeclaration();
        if (!md2)
            continue;

        for (auto overmd: md->foverrides) {
            if (overmd == md2)
                return md2;
        }
    }
    return nullptr;
}

}
