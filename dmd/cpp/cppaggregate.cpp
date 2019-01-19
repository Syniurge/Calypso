// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cpptemplate.h"
#include "cpp/ddmdstructor.h"
#include "cpp/ddmdvisitor.h"
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
#include "llvm/ADT/StringExtras.h"

FuncDeclaration *resolveFuncCall(const Loc &loc, Scope *sc, Dsymbol *s,
        Objects *tiargs,
        Type *tthis,
        Expressions *arguments,
        int flags = 0);
Expression *resolveProperties(Scope *sc, Expression *e);
FuncDeclaration *hasIdentityOpAssign(AggregateDeclaration *ad, Scope *sc);
Dsymbol *search_function(ScopeDsymbol *ad, Identifier *funcid);

namespace cpp
{

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

template<typename AggTy> bool buildAggLayout(AggTy *ad);

void MarkAggregateReferencedImpl(AggregateDeclaration* ad)
{
    auto D = dyn_cast<clang::CXXRecordDecl>(
                    const_cast<clang::RecordDecl*>(getRecordDecl(ad)));
    if (!D)
        return;

    if (D->hasDefinition()) {
        auto& Context = calypso.getASTContext();
        auto& S = calypso.getSema();

        auto Key = Context.getCurrentKeyFunction(D);
        const clang::FunctionDecl* Body;
        if (!Key || (Key->hasBody(Body) && Context.DeclMustBeEmitted(Body))) {
            // As in C++, only mark virtual methods for codegen if the key method is defined
            // If the definition isn't the current TU, assume that methods have been emitted
            // by another TU.
            S.MarkVTableUsed(D->getLocation(), D);

            for (auto s: *ad->members)
                if (s->isFuncDeclaration() && isCPP(s)) {
                    auto fd = static_cast<::FuncDeclaration*>(s);
                    auto MD = dyn_cast<clang::CXXMethodDecl>(getFD(fd));
                    if (MD && MD->isVirtual())
                        MarkFunctionReferenced(fd);
                }

            if (ad->defaultCtor)
                calypso.markSymbolReferenced(ad->defaultCtor);
            if (ad->dtor)
                calypso.markSymbolReferenced(ad->dtor);
        }

        DeclReferencer declReferencer;
        auto sc = ad->_scope;
        if (!sc)
            sc = ad->getModule()->_scope; // FIXME: ad->_scope shouldn't be null, and won't be after the fwdref work
        assert(sc);

        for (auto Field: D->fields())
            if (auto InClassInit = Field->getInClassInitializer())
                declReferencer.Traverse(ad->loc, sc, InClassInit);

        markAggregateReferenced(ad);
    }
}

void LangPlugin::mangleAnonymousAggregate(::AggregateDeclaration* ad, OutBuffer *buf)
{
    auto MangleCtx = pch.MangleCtx;
    auto RD = getRecordDecl(ad);

    // Get a unique id for the anonymous struct.
    unsigned AnonStructId = MangleCtx->getAnonymousStructId(RD);

    llvm::SmallString<8> Str;
    Str += "$_";
    Str += llvm::utostr(AnonStructId);

    buf->printf("%llu", (unsigned long long)Str.size());
    buf->writestring(Str.c_str());
}

StructDeclaration::StructDeclaration(Loc loc, Identifier* id,
                                     const clang::RecordDecl* RD)
{
    construct_StructDeclaration(this, loc, id);
    this->RD = RD;
}

StructDeclaration::StructDeclaration(const StructDeclaration& o)
    : StructDeclaration(o.loc, o.ident, o.RD)
{
}

ClassDeclaration::ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses,
                                   Dsymbols* members, const clang::CXXRecordDecl *RD)
{
    construct_ClassDeclaration(this, loc, id, baseclasses, members);
    this->RD = RD;
}

ClassDeclaration::ClassDeclaration(const ClassDeclaration& o)
    : ClassDeclaration(o.loc, o.ident, nullptr, nullptr, o.RD)
{ // NOTE: baseclasses/members will be duplicated by syntaxCopy, but this makes the copy constructor not doing what it should be doing
}

UnionDeclaration::UnionDeclaration(Loc loc, Identifier* id,
                                     const clang::RecordDecl* RD)
{
    construct_UnionDeclaration(this, loc, id);
    this->RD = RD;
}

UnionDeclaration::UnionDeclaration(const UnionDeclaration& o)
    : UnionDeclaration(o.loc, o.ident, o.RD)
{
}

IMPLEMENT_syntaxCopy(StructDeclaration, RD)
IMPLEMENT_syntaxCopy(ClassDeclaration, RD)
IMPLEMENT_syntaxCopy(UnionDeclaration, RD)

void StructDeclaration::accept(Visitor *v)
{
    auto v_ti = v->_typeid();

    if (v_ti == TI_DsymbolSem1Visitor) { // semantic
        v->visit(this);
        const_cast<clang::RecordDecl*>(RD)->dsym = this;
        tidtor = dtor;
    } else if (v_ti == TI_DsymbolSem3Visitor) { // semantic3
        if (isUsed)
            MarkAggregateReferencedImpl(this);
        v->visit(this);
    } else if (v_ti == TI_Mangler) { // mangle
        if (isAnonymous())
            calypso.mangleAnonymousAggregate(this, static_cast<Mangler*>(v)->buf);
        else
            v->visit(this);
    } else
        v->visit(this);
}

Expression *StructDeclaration::defaultInit(Loc loc)
{
    auto arguments = new Expressions;
    return new_CallExp(loc, new_TypeExp(loc, type), arguments);
}

bool StructDeclaration::mayBeAnonymous()
{
    return true;
}

bool StructDeclaration::determineFields()
{
    if (sizeok != SIZEOKnone)
        return true;

    if (!buildAggLayout(this))
        return false;

    if (sizeok != SIZEOKdone)
        sizeok = SIZEOKfwd;

    return true;
}

bool StructDeclaration::buildLayout()
{
    return buildAggLayout(this);
}

void StructDeclaration::finalizeSize()
{
    ::StructDeclaration::finalizeSize();
    if (!ctor)
        ctor = searchCtor();
    if (ctor)
        zeroInit = 0;
}

void ClassDeclaration::accept(Visitor *v)
{
    auto v_ti = v->_typeid();

    if (v_ti == TI_DsymbolSem1Visitor) { // semantic
        v->visit(this);
        const_cast<clang::CXXRecordDecl*>(RD)->dsym = this;
        tidtor = dtor;
    } else if (v_ti == TI_DsymbolSem3Visitor) { // semantic3
        if (isUsed)
            MarkAggregateReferencedImpl(this);
        v->visit(this);
    } else if (v_ti == TI_Mangler) { // mangle
        if (isAnonymous())
            calypso.mangleAnonymousAggregate(this, static_cast<Mangler*>(v)->buf);
        else
            v->visit(this);
    } else
        v->visit(this);
}

bool ClassDeclaration::mayBeAnonymous()
{
    return true;
}

void ClassDeclaration::addLocalClass(ClassDeclarations *aclasses)
{
    if (isUsed)
        ::ClassDeclaration::addLocalClass(aclasses);
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

        auto Offset = clang::ComputeBaseOffset_(Context, RD, RD2);
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

::CtorDeclaration* hasCopyCtor(AggregateDeclaration* ad, Scope* sc)
{
    if (!ad->ctor)
        return nullptr;

    auto er = new_NullExp(ad->loc, ad->type);    // dummy rvalue
    auto el = new_IdentifierExp(ad->loc, Id::p); // dummy lvalue
    el->type = ad->type;
    Expressions a;
    a.setDim(1);
    auto errors = global.startGagging();
    sc = sc->push();
    sc->tinst = nullptr;
    sc->minst = nullptr;

    a[0] = er;
    auto f = resolveFuncCall(ad->loc, sc, ad->ctor, nullptr, ad->type, &a, 1|8);
    if (!f)
    {
        a[0] = el;
        f = resolveFuncCall(ad->loc, sc, ad->ctor, nullptr, ad->type, &a, 1|8);
    }

    sc = sc->pop();
    global.endGagging(errors);
    if (f)
    {
        if (f->errors)
            return nullptr;
        int varargs;
        auto fparams = f->getParameters(&varargs);
        if (fparams->dim >= 1)
        {
            auto fparam0 = Parameter::getNth(fparams, 0);
            if (fparam0->type->toDsymbol(nullptr) != ad)
                f = nullptr;
        }
    }
    return f ? f->isCtorDeclaration() : nullptr;
}

::CtorDeclaration* StructDeclaration::hasCopyCtor(Scope* sc)
{
    return cpp::hasCopyCtor(this, sc);
}

::CtorDeclaration* ClassDeclaration::hasCopyCtor(Scope* sc)
{
    return cpp::hasCopyCtor(this, sc);
}

template <typename AggTy>
 Expression* buildVarInitializerImpl(AggTy *ad, Scope* sc, ::VarDeclaration* vd, Expression* exp)
{
    if (exp->op == TOKstructliteral)
        return nullptr;

    Loc loc = exp->loc;
    auto ve = new_VarExp(loc, vd);

    if (vd->storage_class & STCtemp) {
        exp = new_ConstructExp(loc, ve, exp);
        return exp;
    }

    // We need to avoid useless temporaries that needlessly complicate codegen and result in added (wrong) dtor calls into AST
    // NOTE: buildVarInitializer must be called before the initializer exp gets semantic'd()
    CallExp* ce = nullptr;

    if (exp->op == TOKcall) {
        auto ce2 = static_cast<CallExp*>(exp);
        unaSemantic(ce2, sc);
        if (ce2->e1->op == TOKtype &&
                    ad->getType() == static_cast<TypeExp*>(ce2->e1)->type->toBasetype())
            ce = ce2; // rewrite T var = T(...) as T var; v.this(...);
    }

    // FIXME: sc->intypeof == 1?

    if (ad->ctor)
    {
        assert(ad->ctor->isDeclaration());
        auto e1 = new_DotVarExp(loc, ve, static_cast<Declaration*>(ad->ctor));

        if (!ce) {
            exp = expressionSemantic(exp, sc);
            exp = resolveProperties(sc, exp);
            
            if (exp->type->constConv(ad->getType()) >= MATCHconst)
                exp = new_ConstructExp(loc, ve, exp); // enables in-place construction
            else
            {
                auto args = new Expressions;
                args->push(exp);

                if (!resolveFuncCall(loc, nullptr, ad->ctor, nullptr, nullptr, args, 1|4))
                    args->pop(); // TODO: error if there'ss no default ctor

                ce = new_CallExp(loc, e1, args);

                if (args->dim == 0)
                    // rewrite to an assignment
                    exp = new_CommaExp(loc, ce,
                                        new_AssignExp(loc, ve, exp));
                else
                    exp = ce;
            }
        } else
            ce->e1 = e1;
    }
    else
    {
        Expression* init = ce
                    ? new_StructLiteralExp(loc, ad, ce->arguments, ad->getType())
                    : ad->getType()->defaultInitLiteral(loc);
        auto ae = new_BlitExp(loc, ve, init);
        if (ce)
            exp = ae;
        else
            exp = new_AssignExp(loc, ae, exp);
    }
    return exp;
}

Expression* StructDeclaration::buildVarInitializer(Scope* sc, ::VarDeclaration* vd, Expression* exp)
{
    return buildVarInitializerImpl(this, sc, vd, exp);
}

Expression* ClassDeclaration::buildVarInitializer(Scope* sc, ::VarDeclaration* vd, Expression* exp)
{
    return buildVarInitializerImpl(this, sc, vd, exp);
}

Expression *ClassDeclaration::defaultInit(Loc loc)
{
    if (!defaultCtor)
        return ::ClassDeclaration::defaultInit(loc);

    auto arguments = new Expressions;
    return new_CallExp(loc, new_TypeExp(loc, type), arguments);
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

bool ClassDeclaration::determineFields()
{
    if (sizeok != SIZEOKnone)
        return true;

    if (!buildAggLayout(this))
        return false;

    if (sizeok != SIZEOKdone)
        sizeok = SIZEOKfwd;

    return true;
}

bool ClassDeclaration::buildLayout()
{
    return buildAggLayout(this);
}

bool UnionDeclaration::mayBeAnonymous()
{
    return true;
}

bool UnionDeclaration::determineFields()
{
    if (sizeok != SIZEOKnone)
        return true;

    if (!buildAggLayout(this))
        return false;

    if (sizeok != SIZEOKdone)
        sizeok = SIZEOKfwd;

    return true;
}

bool UnionDeclaration::buildLayout()
{
    return buildAggLayout(this);
}

AnonDeclaration::AnonDeclaration(Loc loc, bool isunion, Dsymbols* decl)
{
    construct_AnonDeclaration(this, loc, isunion, decl);
}

Dsymbol* AnonDeclaration::syntaxCopy(Dsymbol* s)
{
    assert(!s);
    auto a = new AnonDeclaration(loc, isunion, decl);
    a->AnonField = AnonField;
    return a;
}

Expression *LangPlugin::callCpCtor(Scope *sc, Expression *e)
{
    Type* tv = e->type->baseElemOf();
    assert(isAggregateValue(tv));
    auto sym = getAggregateSym(tv);

    if (!hasCopyCtor(sym, sc))
        return nullptr;

    auto arguments = new Expressions;
    arguments->push(e);
    e = new_CallExp(e->loc, new_TypeExp(e->loc, tv), arguments);
    return expressionSemantic(e, sc);
}

::DtorDeclaration *LangPlugin::buildDtor(::AggregateDeclaration *ad, Scope *sc)
{
    assert(ad->dtors.dim < 2);

    if (ad->dtors.empty())
        return nullptr; // forward reference

    // On aggregate destruction the GC looks for a "__xdtor" member
    auto _alias = new_AliasDeclaration(Loc(), Id::__xdtor, ad->dtors[0]);
    dsymbolSemantic(_alias, sc);
    ad->members->push(_alias);
    _alias->addMember(sc, ad); // add to symbol table

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

::FuncDeclaration *LangPlugin::searchOpEqualsForXopEquals(::StructDeclaration *sd, Scope *sc)
{
    if (Dsymbol *eq = search_function(sd, Id::eq))
    {
        if (::FuncDeclaration *fd = eq->isFuncDeclaration())
        {
            TypeFunction *tfeqptr;
            {
                Scope* scx = Scope::alloc();

                /* extern(C++) const bool opEquals(scope ref const S s);
                */
                Parameters *parameters = new Parameters;
                parameters->push(new_Parameter(STCscope | STCref | STCconst, sd->type, NULL, NULL, NULL));
                tfeqptr = new_TypeFunction(parameters, Type::tbool, 0, LINKcpp);
                tfeqptr->mod = MODconst;
                tfeqptr = (TypeFunction *)typeSemantic(tfeqptr, Loc(), scx);

                scx->pop();
            }
            fd = fd->overloadExactMatch(tfeqptr);
            if (fd)
                return fd;
        }
    }
    return nullptr;
}

template <typename AggTy>
 bool buildAggLayout(AggTy *ad)
{
    assert(isCPP(ad));

    if (ad->layoutQueried)
        return true;

    if (ad->RD->isInvalidDecl() || !ad->RD->getDefinition())
    {
       // if it's a forward reference, consider the record empty
        ad->structsize = 1;
        ad->alignsize = 1;
        return true;
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

                if (vd->_scope)
                    dsymbolSemantic(vd, nullptr);

                auto c_vd = static_cast<VarDeclaration*>(vd);
                auto FD = dyn_cast<clang::FieldDecl>(c_vd->VD);

                if (!FD)
                    continue;

                auto fldIdx = FD->getFieldIndex();
                vd->offset = baseoffset + Layout.getFieldOffset(fldIdx) / 8;
                c_vd->offsetInBits = baseoffset * 8 + Layout.getFieldOffset(fldIdx);

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
    return true;
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
            if (!isCPP(b->sym)) // skip Object
                continue;

            auto result = findMethod(b->sym, FD);
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

static bool& getIsUsed(::AggregateDeclaration* ad)
{
    if (ad->isClassDeclaration())
        return static_cast<cpp::ClassDeclaration*>(ad)->isUsed;
    else
        return static_cast<cpp::StructDeclaration*>(ad)->isUsed;
}

void MarkAggregateReferenced(::AggregateDeclaration* ad)
{
    auto& isUsed = getIsUsed(ad);
    if (isUsed)
        return;
    isUsed = true;

    if (ad->semanticRun >= PASSsemanticdone)
        MarkAggregateReferencedImpl(ad);
}

}
