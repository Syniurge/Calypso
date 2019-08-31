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
#include "clang/Sema/SemaDiagnostic.h"
#include "llvm/ADT/StringExtras.h"

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

FuncDeclaration *resolveFuncCall(const Loc &loc, Scope *sc, Dsymbol *s,
        Objects *tiargs,
        Type *tthis,
        Expressions *arguments,
        int flags = 0);
Expression *resolveProperties(Scope *sc, Expression *e);
FuncDeclaration *hasIdentityOpAssign(AggregateDeclaration *ad, Scope *sc);
Dsymbol *search_function(ScopeDsymbol *ad, Identifier *funcid);

void MarkAggregateReferencedImpl(AggregateDeclaration* ad)
{
    using namespace cpp;

    auto D = dyn_cast<clang::CXXRecordDecl>(
                    const_cast<clang::RecordDecl*>(getRecordDecl(ad)));

    if (D && D->hasDefinition()) {
        auto& Context = calypso.getASTContext();
        auto& S = calypso.getSema();

        auto Key = Context.getCurrentKeyFunction(D);
        const clang::FunctionDecl* Body;
        if (!Key || (Key->hasBody(Body) && Context.DeclMustBeEmitted(Body))) {
            // As in C++, only mark virtual methods for codegen if the key method is defined
            // If the definition isn't the current TU, assume that methods have been emitted
            // by another TU.
            S.MarkVTableUsed(D->getLocation(), D);

            for (auto MD: D->methods())
                if (MD->isVirtual()) {
                    auto md = static_cast<::FuncDeclaration*>(
                                    DeclMapper(ad).dsymForDecl(MD));
                    MarkFunctionReferenced(md);
                }

            if (ad->defaultCtor)
                calypso.markSymbolReferenced(ad->defaultCtor);
            if (ad->dtor)
                calypso.markSymbolReferenced(ad->dtor);
        }

//         DeclReferencer declReferencer(ad);
//
//         for (auto Field: D->fields())
//             if (auto InClassInit = Field->getInClassInitializer())
//                 declReferencer.Traverse(ad->loc, InClassInit);

        markAggregateReferenced(ad);
    }
}

namespace cpp
{

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

void StructDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    Dsymbol::addMember(sc, sds);
}

void ClassDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    Dsymbol::addMember(sc, sds);
}

void UnionDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    Dsymbol::addMember(sc, sds);
}

template <typename AggTy>
inline decltype(AggTy::_Def) ad_Definition(AggTy* ad)
{
    if (!ad->_Def)
    {
        auto& Context = calypso.getASTContext();
        auto& S = calypso.getSema();

        if (S.RequireCompleteType(ad->RD->getLocation(), Context.getRecordType(ad->RD),
                                  clang::diag::err_incomplete_type))
            ad->error("No definition available");

        if (!ad->RD->isCompleteDefinition() && ad->RD->getDefinition())
            ad->_Def = ad->RD->getDefinition();
        else
            ad->_Def = ad->RD;
    }

    assert(cast<clang::NamedDecl>(getCanonicalDecl(ad->_Def))->d->sym == ad); // making sure that the canon decl is still the same (FIXME might be completely unnecessary)
    return ad->_Def;
}

const clang::RecordDecl *StructDeclaration::Definition()
{
    return ad_Definition(this);
}

const clang::CXXRecordDecl *ClassDeclaration::Definition()
{
    return ad_Definition(this);
}

const clang::RecordDecl *UnionDeclaration::Definition()
{
    return ad_Definition(this);
}

template <typename AggTy>
inline Dsymbol* ad_search(AggTy* ad, const Loc &loc, Identifier *ident, int flags)
{
    if (auto s = ad->ScopeDsymbol::search(loc, ident, flags))
        return s;

    auto Def = ad->Definition();
    if (!Def)
        return nullptr;

    auto Name = calypso.toDeclarationName(ident);
    for (auto Match: Def->lookup(Name))
        dsymForDecl(ad, Match);

    auto CRD = dyn_cast<clang::CXXRecordDecl>(ad->RD);
    if (CRD && !CRD->isUnion())
    {
        auto& S = calypso.getSema();
        auto _CRD = const_cast<clang::CXXRecordDecl *>(CRD);

        // NOTE: Would mapping only non-trivial special members be preferable?
        // It probably would make mapping more subject to unexpected variations..

        if (ident == Id::ctor)
        {
            dsymForDecl(ad, S.LookupDefaultConstructor(_CRD));

            for (int i = 0; i < 2; i++)
                dsymForDecl(ad, S.LookupCopyingConstructor(_CRD, i ? clang::Qualifiers::Const : 0));
        }
        else if (ident == Id::dtor)
        {
            dsymForDecl(ad, S.LookupDestructor(_CRD));
        }
        else if (ident == Id::assign)
        {
            for (int i = 0; i < 2; i++)
                for (int j = 0; j < 2; j++)
                    for (int k = 0; k < 2; k++)
                        dsymForDecl(ad, S.LookupCopyingAssignment(_CRD, i ? clang::Qualifiers::Const : 0,
                                    j ? true : false, k ? clang::Qualifiers::Const : 0));
        }
    }

    return ad->ScopeDsymbol::search(loc, ident, flags);
}

Dsymbol *StructDeclaration::search(const Loc &loc, Identifier *ident, int flags)
{
    return ad_search(this, loc, ident, flags);
}

Dsymbol *ClassDeclaration::search(const Loc &loc, Identifier *ident, int flags)
{
    auto s = ad_search(this, loc, ident, flags);

    if (!s)
        for (auto b: *baseclasses)
            if (b->sym)
            {
                s = b->sym->search(loc, ident, flags);
                if (s)
                    break;
            }

    return s;
}

Dsymbol *UnionDeclaration::search(const Loc &loc, Identifier *ident, int flags)
{
    return ad_search(this, loc, ident, flags);
}

// TODO replace by generator pattern/input iterator?

template <typename AggTy>
inline void ad_complete(AggTy* ad)
{
    if (ad->membersCompleted)
        return;
    ad->membersCompleted = true;

    // Force declaration of implicit special members
    ad->search(ad->loc, Id::ctor);
    ad->search(ad->loc, Id::dtor);
    ad->search(ad->loc, Id::assign);

    Dsymbols* newMembers = new Dsymbols;
    newMembers->reserve(ad->members->dim);

    auto Canon = ad->RD->getCanonicalDecl();

    // Add specific decls: fields, vars, tags, templates, typedefs
    for (auto M: ad->Definition()->decls())
    {
        if (cast<clang::Decl>(M->getDeclContext())->getCanonicalDecl() != Canon)
            continue; // only map declarations that are semantically within the RecordDecl

        if (!isa<clang::FieldDecl>(M) && !isa<clang::VarDecl>(M) &&
              !isa<clang::FunctionDecl>(M) && !isa<clang::TagDecl>(M) &&
              !isa<clang::RedeclarableTemplateDecl>(M) && !isa<clang::TypedefNameDecl>(M))
            continue;

        newMembers->push(dsymForDecl(ad, M));
    }

    delete ad->members;
    ad->members = newMembers;
}

void StructDeclaration::complete()
{
    ad_complete(this);
}

void ClassDeclaration::complete()
{
    ad_complete(this);
}

void UnionDeclaration::complete()
{
    ad_complete(this);
}

void StructDeclaration::accept(Visitor *v)
{
    auto v_ti = v->_typeid();

    if (v_ti == TI_Mangler) { // mangle
        if (isAnonymous())
            calypso.mangleAnonymousAggregate(this, static_cast<Mangler*>(v)->buf);
        else
            v->visit(this);
    } else
        v->visit(this);
}

template <typename AggTy>
void ad_determineSize(AggTy *ad)
{
    assert(ad->sizeok != SIZEOKdone);
    ad->sizeok = SIZEOKdone;

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

    typedef clang::DeclContext::specific_decl_iterator<clang::ValueDecl> Value_iterator;
    for (Value_iterator I(ad->RD->decls_begin()), E(ad->RD->decls_end()); I != E; I++)
    {
        if (!isa<clang::FieldDecl>(*I) && !isa<clang::IndirectFieldDecl>(*I)/* &&
            !isa<clang::MSPropertyDecl>(*I) (TODO)*/)
            continue;

        auto Field = *I;
        auto vd = static_cast<VarDeclaration*>(dsymForDecl(ad, Field));
        ad->fields.push(vd);

        vd->offsetInBits = Context.getFieldOffset(Field);
        vd->offset = vd->offsetInBits / 8;
    }

    auto CRD = dyn_cast<clang::CXXRecordDecl>(ad->RD);
    if (auto sd = ad->isStructDeclaration())
        if (!CRD || CRD->ctor_begin() == CRD->ctor_end())
            sd->zeroInit = true;
}

// NOTE: size() gets called to "determine fields", but shouldn't the two be separate?
d_uns64 StructDeclaration::size(const Loc &loc)
{
    if (sizeok != SIZEOKdone)
        ad_determineSize(this);
    return structsize;
}

d_uns64 ClassDeclaration::size(const Loc &loc)
{
    if (sizeok != SIZEOKdone)
        ad_determineSize(this);
    return structsize;
}

d_uns64 UnionDeclaration::size(const Loc &loc)
{
    if (sizeok != SIZEOKdone)
        ad_determineSize(this);
    return structsize;
}

Expression *StructDeclaration::defaultInit(Loc loc)
{
    auto arguments = new Expressions;
    return new_CallExp(loc, new_TypeExp(loc, type), arguments);
}

void ClassDeclaration::accept(Visitor *v)
{
    auto v_ti = v->_typeid();

    if (v_ti == TI_Mangler) { // mangle
        if (isAnonymous())
            calypso.mangleAnonymousAggregate(this, static_cast<Mangler*>(v)->buf);
        else
            v->visit(this);
    } else
        v->visit(this);
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

    if (auto ctor = ad->search(vd->loc, Id::ctor))
    {
        auto e1 = new_DotIdExp(loc, ve, Id::ctor);

        if (!ce) {
            exp = expressionSemantic(exp, sc);
            exp = resolveProperties(sc, exp);

            if (exp->type->constConv(ad->getType()) >= MATCHconst)
                exp = new_ConstructExp(loc, ve, exp); // enables in-place construction
            else
            {
                auto args = new Expressions;
                args->push(exp);

                if (!resolveFuncCall(loc, nullptr, ctor, nullptr, nullptr, args, 1|4))
                    args->pop(); // TODO: error if there's no default ctor

                ce = new_CallExp(loc, e1, args);

                if (args->dim == 0)
                    // rewrite to an assignment
                    exp = new_CommaExp(loc, ce, new_AssignExp(loc, ve, exp));
                else
                    exp = ce;
            }
        } else
            ce->e1 = e1;
    }
    else
    {
        ad->size(loc); // to determine fields

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

// NOTE: the "D" vtbl isn't used unless a D class inherits from a C++ one
void ClassDeclaration::buildVtbl()
{
    if (auto bcd = isClassDeclarationOrNull(baseClass))
    {
        static_cast<cpp::ClassDeclaration*>(bcd)->buildVtbl();

        vtbl.setDim(bcd->vtbl.dim);
        memcpy(vtbl.tdata(), bcd->vtbl.tdata(), sizeof(void*) * vtbl.dim);
    }

    clang::CXXFinalOverriderMap FinalOverriders;
    RD->getFinalOverriders(FinalOverriders);

    for (const auto &Overrider : FinalOverriders)
    {
        auto OverMD = Overrider.second.begin()->second.front().Method;
        auto md = static_cast<FuncDeclaration*>(dsymForDecl(this, OverMD));
        if (!md)
            continue;

        if (md->vtblIndex) // FIXME? in C++ a method can override two base methods, so can't be represented by vtblIndex
            continue;

        auto vi = md->findVtblIndex(&vtbl, vtbl.dim);
        if (vi < 0)
        {
            md->vtblIndex = vtbl.dim;
            vtbl.push(md);
        }
        else
            md->vtblIndex = vi;
    }
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

void AnonDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    Dsymbol::addMember(sc, sds); // do not re-add the contents to parent's members
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
