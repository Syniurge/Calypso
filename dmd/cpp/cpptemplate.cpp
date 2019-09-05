// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cpptemplate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/ddmdstructor.h"
#include "cpp/ddmdvisitor.h"
#include "aggregate.h"
#include "enum.h"
#include "id.h"
#include "scope.h"

#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/TemplateDeduction.h"

size_t correspondingParamIdx(size_t argi, TemplateDeclaration* tempdecl, Objects* tiargs);

struct RefParamPartialOrderingComparison;
namespace clang {
    bool isAtLeastAsSpecializedAs_(Sema &S,
                                     SourceLocation Loc,
                                     FunctionTemplateDecl *FT1,
                                     FunctionTemplateDecl *FT2,
                                     TemplatePartialOrderingContext TPOC,
                                     unsigned NumCallArguments1,
                                     unsigned TDF);
}

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::isa;

TemplateDeclaration::TemplateDeclaration(Loc loc, Identifier* id,
                    TemplateParameters* parameters,
                    Dsymbols* decldefs,
                    const clang::NamedDecl *TempOrSpec)
{
    construct_TemplateDeclaration(this, loc, id, parameters, nullptr, decldefs);
    this->TempOrSpec = TempOrSpec;
    this->constraint = new_NullExp(loc); // HACK to grant Sema another check e.g for templates with std::enable_if
}

TemplateDeclaration::TemplateDeclaration(const TemplateDeclaration &o)
    : TemplateDeclaration(o.loc, o.ident, o.parameters, o.members, o.TempOrSpec)
{
    this->constraint = new_NullExp(loc);
}

IMPLEMENT_syntaxCopy(TemplateDeclaration, TempOrSpec)

static void fillTemplateArgumentListInfo(Loc loc, Scope *sc, clang::TemplateArgumentListInfo& Args,
                                Objects *tiargs, const clang::RedeclarableTemplateDecl *Temp,
                                DeclMapper& mapper, ExprMapper& expmap)
{
    auto& Context = calypso.getASTContext();

    SpecValue spec(mapper);
    getIdentifierOrNull(Temp, &spec);

    std::function<void(RootObject* o)>
        addArg = [&] (RootObject* o)
    {
        auto ta = isType(o);
        auto ea = isExpression(o);
        auto sa = isDsymbol(o);
        auto tupa = isTuple(o);

        if (ta)
        {
            auto T = mapper.toType(loc, ta, sc);
            auto DI = Context.getTrivialTypeSourceInfo(T);

            clang::TemplateArgumentLoc Loc(clang::TemplateArgument(T), DI);
            Args.addArgument(Loc);
        }
        else if (ea)
        {
            auto E = expmap.toExpression(ea);

            clang::TemplateArgumentLoc Loc(clang::TemplateArgument(E), E);
            Args.addArgument(Loc);
        }
        else if (sa)
        {
            auto tempdecl = sa->isTemplateDeclaration();
            assert(tempdecl && isCPP(tempdecl));

            auto c_td = static_cast<cpp::TemplateDeclaration *>(tempdecl);
            auto Temp = c_td->getPrimaryTemplate();
            clang::TemplateName Name(Temp);

            clang::TemplateArgumentLoc Loc(clang::TemplateArgument(Name),
                                    clang::NestedNameSpecifierLoc(),
                                    Temp->getLocation(), clang::SourceLocation());
            Args.addArgument(Loc);
        }
        else if (tupa)
        {
            for (auto tupobj: tupa->objects)
                addArg(tupobj);
        }
        else
            assert(false && "unhandled template arg C++ -> D conversion");
    };

    // See translateTemplateArgument() in SemaTemplate.cpp
    for (unsigned i = 0; i < tiargs->dim; i++)
    {
        if (i == 0 && spec)
            continue; // skip the first parameter of opUnary/opBinary/opOpAssign/...

        auto o = (*tiargs)[i];
        if (!o)
            break;
        addArg(o);
    }
}

const clang::TemplateParameterList *getPartialTemplateSpecParameters(const clang::Decl *D)
{
    if (auto PartialCTS = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(D))
        return PartialCTS->getTemplateParameters();
    else if (auto PartialVTS = dyn_cast<clang::VarTemplatePartialSpecializationDecl>(D))
        return PartialVTS->getTemplateParameters();

    return nullptr;
}

const clang::TemplateArgumentList *getTemplateArgs(const clang::Decl *D)
{
    if (auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
        return &CTSD->getTemplateArgs();
    else if (auto FD = dyn_cast<clang::FunctionDecl>(D))
        return FD->getTemplateSpecializationArgs();
    else if (auto VTSD = dyn_cast<clang::VarTemplateSpecializationDecl>(D))
        return &VTSD->getTemplateArgs();

    return nullptr;
}

const clang::TemplateArgumentList *getTemplateArgs(const clang::TemplateSpecializationType *T)
{
    auto& Context = calypso.getASTContext();
    return clang::TemplateArgumentList::CreateCopy(Context, T->template_arguments());
}

const clang::TemplateArgumentList *getTemplateArgs(TemplateInstUnion U)
{
    if (auto D = U.dyn_cast<clang::NamedDecl*>())
        return getTemplateArgs(D);
    else
        return getTemplateArgs(U.get<const clang::TemplateSpecializationType*>());
}

const clang::TemplateArgumentList *getTemplateInstantiationArgs(const clang::Decl *D)
{
    if (auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
        return &CTSD->getTemplateInstantiationArgs();
    else if (auto FD = dyn_cast<clang::FunctionDecl>(D))
        return FD->getTemplateSpecializationArgs();
    else if (auto VTSD = dyn_cast<clang::VarTemplateSpecializationDecl>(D))
        return &VTSD->getTemplateInstantiationArgs();

    return nullptr;
}

const clang::TemplateArgumentList *getTemplateInstantiationArgs(const clang::TemplateSpecializationType *T)
{
    return getTemplateArgs(T);
}

const clang::TemplateArgumentList *getTemplateInstantiationArgs(TemplateInstUnion U)
{
    if (auto D = U.dyn_cast<clang::NamedDecl*>())
        return getTemplateInstantiationArgs(D);
    else
        return getTemplateInstantiationArgs(U.get<const clang::TemplateSpecializationType*>());
}

clang::FunctionDecl *instantiateFunctionDeclaration(clang::TemplateArgumentListInfo& Args,
                                clang::FunctionTemplateDecl *FuncTemp)
{
    auto& S = calypso.getSema();

    // Converts TemplateArgumentListInfo to something suitable for TemplateArgumentList
    llvm::SmallVector<clang::TemplateArgument, 4> Converted;
    if (S.CheckTemplateArgumentList(FuncTemp, FuncTemp->getLocation(), Args,
                                    false, Converted))
        return nullptr;

    clang::TemplateArgumentList ArgList(clang::TemplateArgumentList::OnStack,
                            Converted);
    clang::MultiLevelTemplateArgumentList MultiList(ArgList);

    // Instantiate the declaration
    clang::Sema::InstantiatingTemplate Instantiating(S,
                    FuncTemp->getLocation(), FuncTemp->getTemplatedDecl());
    if (Instantiating.isInvalid())
        return nullptr;

    return llvm::cast_or_null<clang::FunctionDecl>(
                    S.SubstDecl(FuncTemp->getTemplatedDecl(),
                                FuncTemp->getDeclContext(), MultiList));
}

// C++ doesn't have constraints but in function calls SFINAE with std::enable_if<> and the like may be used to weed out candidate overloads,
// whereas D expects at least the instantiated symbol to have a valid type.
// evaluateConstraint is a good place to make Sema check whether the instantiated decl is valid or not
// FIXME: may be actually unneeded since earlyFunctionValidityCheck appears to be the right time to check
bool TemplateDeclaration::evaluateConstraint(::TemplateInstance* ti, Scope* sc, Scope* paramscope,
                                            Objects* dedtypes, ::FuncDeclaration* fd)
{
    if (isForeignInstance(ti))
        return true; // constraint was already evaluated (and the instantiation attempt is done with the primary template args anyway)

    auto& S = calypso.getSema();
    auto& Diags = calypso.getDiagnostics();
    auto Temp = getPrimaryTemplate();

    DeclMapper mapper(sc->minst, sc->minst);
    ExprMapper expmap(mapper);

    clang::TemplateArgumentListInfo Args;
    fillTemplateArgumentListInfo(ti->loc, sc, Args, dedtypes, Temp, mapper, expmap);

    bool instSuccess = true;

    if (isa<clang::ClassTemplateDecl>(Temp) ||
            isa<clang::TypeAliasTemplateDecl>(Temp))
    {
        clang::TemplateName Name(Temp);
        instSuccess = !S.CheckTemplateIdType(Name, Temp->getLocation(), Args).isNull();
    }
    else if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(Temp))
    {
        instSuccess = instantiateFunctionDeclaration(Args, FuncTemp);
    }

    if (!instSuccess) {
        assert(false); // FIXME TEMPORARY? still necessary after going lazy?
        Diags.Reset();
    }

    return instSuccess;
}

bool TemplateDeclaration::earlyFunctionValidityCheck(::TemplateInstance* ti, Scope* sc, Objects* dedtypes)
{
    return evaluateConstraint(ti, sc, nullptr, dedtypes, nullptr);
}

void TemplateDeclaration::prepareBestMatch(::TemplateInstance* ti, Scope* sc, Expressions* fargs)
{
    if (!isa<clang::FunctionTemplateDecl>(TempOrSpec) && !ti->havetempdecl)
    {
        ti->havetempdecl = true;
        ti->tempdecl = primaryTemplate();
    }
}

static MATCH matchWithInstanceNonTemplated(TemplateDeclaration* tempdecl, Scope *sc, ::TemplateInstance *ti,
                                             Objects *dedtypes, Expressions *fargs, int flag)
{
    dedtypes->zero();

    // If more arguments than parameters, no match
    if (ti->tiargs->dim > tempdecl->parameters->dim)
        return MATCHnomatch;

    assert(dedtypes->dim == tempdecl->parameters->dim);
    assert(dedtypes->dim >= ti->tiargs->dim);

    // Attempt type deduction
    MATCH m = MATCHexact;
    size_t argi = 0; // CALYPSO
    for (size_t i = 0; i < dedtypes->dim; i++)
    {
        auto tp = (*tempdecl->parameters)[i];

        MATCH m2 = tp->matchArg(ti->loc, sc /* HACK-ish, everything is already semantic'd */,
                                ti->tiargs, i, &argi /* CALYPSO */,
                                tempdecl->parameters, dedtypes, nullptr);
        if (m2 == MATCHnomatch)
            return MATCHnomatch;

        if (m2 < m)
            m = m2;
    }

    if (!flag)
    {
        /* Any parameter left without a type gets the type of
            * its corresponding arg
            */
        for (size_t i = 0; i < dedtypes->dim; i++)
        {
            if (!(*dedtypes)[i])
            {
                assert(i < ti->tiargs->dim);
                (*dedtypes)[i] = static_cast<Type*>((*ti->tiargs)[i]);
            }
        }
    }

    return m;
}

MATCH TemplateDeclaration::matchWithInstance(Scope *sc, ::TemplateInstance *ti,
                                             Objects *dedtypes, Expressions *fargs, int flag)
{
    // Special case of a wrapped non-templated overloaded operator
    if (isNonTemplateWrapper())
        return matchWithInstanceNonTemplated(this, sc, ti, dedtypes, fargs, flag);

    // Else, give only the primary template a chance to match
    // The "best matching" is done dy Sema, and then foreignInstance corrects ti->tempdecl
    assert(isa<clang::RedeclarableTemplateDecl>(TempOrSpec));

    TemplateInstUnion Inst;
    assert(!hasExistingClangInst(ti));

    MATCH m = MATCHexact;

    if (isa<clang::FunctionTemplateDecl>(TempOrSpec))
        m = functionTemplateMatch(ti, fargs, Inst);
    else {
        Inst = getClangInst(sc, ti, ti->tiargs);
        if (!Inst)
            m = MATCHnomatch;
    }

    if (m == MATCHnomatch || flag == 1) // 1 means it's from TemplateDeclaration::leastAsSpecialized
        return m;

    std::unique_ptr<Objects> cpptdtypes(tdtypesFromInst(sc, Inst, isForeignInstance(ti)));
    assert(cpptdtypes->dim == dedtypes->dim);
    memcpy(dedtypes->tdata(), cpptdtypes->tdata(), dedtypes->dim * sizeof(void*));

    return m;
}

Objects* TemplateDeclaration::tdtypesFromInst(Scope* sc, TemplateInstUnion Inst, bool forForeignInstance)
{
    DeclMapper mapper(sc->minst, sc->minst);

    Objects* cpptdtypes;

    if (!isNonTemplateWrapper())
    {
        auto InstArgs = forForeignInstance ? getTemplateInstantiationArgs(Inst)
                                        : getTemplateArgs(Inst);

        const clang::TemplateParameterList* ParamList;
        if (auto PartialSpec = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(TempOrSpec))
            ParamList = PartialSpec->getTemplateParameters();
        else
            ParamList = getPrimaryTemplate()->getTemplateParameters();

        cpptdtypes = mapper.fromTemplateArguments<true>(loc, InstArgs, ParamList);
    }
    else
        cpptdtypes = new Objects; // special case for non-templated overloaded operators

    SpecValue spec(mapper);
    if (Inst.is<clang::NamedDecl*>())
        getIdentifier(Inst.get<clang::NamedDecl*>(), &spec, true);
    if (spec)
        cpptdtypes->shift(spec.toTemplateArg(loc));

    if (isVariadic() && cpptdtypes->dim == parameters->dim - 1)
        cpptdtypes->push(new_Tuple());
    if (cpptdtypes->dim == parameters->dim + 1) { // might happen for explicit spec where the pack has zero argument. This feels like a hack, needs cleanup.
        auto tup = isTuple(cpptdtypes->data[0]);
        assert(tup && tup->objects.dim == 0);
        cpptdtypes->dim--;
    }

    return cpptdtypes;
}

// In some cases DMD's leastAsSpecialized isn't enough, for ex. to disambiguate "this(_Tp)(vector!(vector!_Tp))" over "this(_Tp)(vector!_Tp)"
// (and in that particular case, we can't even supply explicit template arguments to the ctor..)
// Fortunately Sema has a method helping refine DMD's result
MATCH TemplateDeclaration::leastAsSpecialized(Scope* sc, ::TemplateDeclaration* td2, Expressions* fargs)
{
    auto Prim1 = getPrimaryTemplate();
    auto Prim2 = isCPP(td2) ? static_cast<cpp::TemplateDeclaration*>(td2)->getPrimaryTemplate() : nullptr;

    if (!Prim1 && !Prim2)
        return ::TemplateDeclaration::leastAsSpecialized(sc, td2, fargs); // both are non-templated overloaded operators
    else if (!Prim1 || !Prim2)
        return Prim2 ? MATCHexact : MATCHnomatch; // only one of the two is a templated overloaded operator, the non-templated one is always the more specialized

    auto FuncTemp1 = cast<clang::FunctionTemplateDecl>(Prim1);
    auto FuncTemp2 = cast<clang::FunctionTemplateDecl>(Prim2);

    auto& Sema = calypso.getSema();

    const unsigned TDF_IgnoreQualifiers = 0x02;
    auto Better1 = clang::isAtLeastAsSpecializedAs_(Sema, clang::SourceLocation(), FuncTemp1, FuncTemp2,
                                isa<clang::CXXConversionDecl>(FuncTemp1->getTemplatedDecl())? clang::TPOC_Conversion : clang::TPOC_Call,
                                fargs->dim, TDF_IgnoreQualifiers);
    if (Better1)
        return MATCHexact;

    return MATCHnomatch;
}

Dsymbol* TemplateDeclaration::wrappedNonTemplateSymbol()
{
    if (members)
        return (*members)[0];

    DeclMapper mapper(this);
    members = mapper.VisitDecl(TempOrSpec, DeclMapper::UnwrapNonTemplatedFunction);
    assert(members && members->dim);

    return (*members)[0];
}

Dsymbols* TemplateDeclaration::copySyntaxTree(::TemplateInstance *ti)
{
    assert(isForeignInstance(ti));
    auto c_ti = static_cast<cpp::TemplateInstance*>(ti);

    assert(!ti->members); // members were already set during decl mapping??

    if (isNonTemplateWrapper())
    {
        c_ti->aliasdecl = wrappedNonTemplateSymbol();
        return members;
    }

    auto Inst = c_ti->Inst.get<clang::NamedDecl*>();

    DeclMapper mapper(ti->minst, ti->minst);
    mapper.VisitDecl(Inst, DeclMapper::UnwrapNonTemplatedFunction);

    auto a = new Dsymbols;
    if (auto inst = Inst->d->sym)
    {
        a->push(inst);
        c_ti->aliasdecl = inst;
    }
    return a;
}

MATCH TemplateDeclaration::functionTemplateMatch(::TemplateInstance *ti, Expressions *fargs,
                                                 TemplateInstUnion& Inst)
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.getSema();

    clang::SourceLocation Loc;
    clang::sema::TemplateDeductionInfo DedInfo(Loc);

    bool isConversion = ident == Id::_cast;

    DeclMapper mapper(ti->minst, ti->minst);
    ExprMapper expmap(mapper);

    auto FunctionTemplate = cast<clang::FunctionTemplateDecl>(TempOrSpec);

    clang::TemplateArgumentListInfo ExplicitTemplateArgs;
    if (ti->tiargs && !isConversion)
        fillTemplateArgumentListInfo(ti->loc, /*sc=*/ nullptr, ExplicitTemplateArgs, ti->tiargs,
                                     FunctionTemplate, mapper, expmap);

    clang::FunctionDecl *Specialization;

    if (isConversion)
    {
        assert(ti->tiargs && ti->tiargs->dim == 1);
        Type* to = isType((*ti->tiargs)[0]);
        clang::QualType To = mapper.toType(ti->loc, to, /*sc=*/ nullptr);

        clang::CXXConversionDecl *Conversion;

        if (S.DeduceTemplateArguments(const_cast<clang::FunctionTemplateDecl*>(FunctionTemplate),
                    To, Conversion, DedInfo))
            return MATCHnomatch;
        Specialization = Conversion;
    }
    else  if (fargs) {
        llvm::SmallVector<clang::Expr*, 4> Args;
        for (auto farg: *fargs) {
            // toType won't take dynamic arrays, but anticipate implicit conversions
            auto argty = farg->type;
            if (argty->ty == Tarray)
                argty = argty->nextOf()->pointerTo();

            auto ArgTy = mapper.toType(ti->loc, argty, /*sc=*/ nullptr);
            auto DummyExpr = new (Context) clang::OpaqueValueExpr(Loc, ArgTy,
                                        farg->isLvalue() ? clang::VK_LValue : clang::VK_RValue);
            Args.push_back(DummyExpr);
        }

        if (S.DeduceTemplateArguments(const_cast<clang::FunctionTemplateDecl*>(FunctionTemplate),
                    &ExplicitTemplateArgs, Args, Specialization, DedInfo, false,
                    [](llvm::ArrayRef<clang::QualType>){ return false; }))
            return MATCHnomatch;
    } else if (S.DeduceTemplateArguments(const_cast<clang::FunctionTemplateDecl*>(FunctionTemplate),
                    &ExplicitTemplateArgs, Specialization, DedInfo))
        return MATCHnomatch;

    Inst = Specialization;
    return MATCHexact;
}

MATCH TemplateDeclaration::deduceFunctionTemplateMatch(::TemplateInstance *ti, Scope *sc,
                            ::FuncDeclaration *&fd, Type *tthis, Expressions *fargs)
{
    auto& S = calypso.getSema();

    if (isNonTemplateWrapper())
        return ::TemplateDeclaration::deduceFunctionTemplateMatch(ti, sc, fd, tthis, fargs);

    DeclMapper mapper(sc->minst, sc->minst);

    TemplateInstUnion Inst;

    if (functionTemplateMatch(ti, fargs, Inst) != MATCHexact)
        return MATCHnomatch;

    MATCH match = MATCHexact;
    MATCH matchTiargs = MATCHexact;

    auto dedtypes = tdtypesFromInst(sc, Inst, false);
    ti->tdtypes.dim = dedtypes->dim;
    ti->tdtypes.data = dedtypes->data;
    ti->tiargs = &ti->tdtypes;

    // HACK
    auto FuncInst = cast<clang::FunctionDecl>(Inst.get<clang::NamedDecl*>());
    auto FPT = FuncInst->getType()->castAs<clang::FunctionProtoType>();

    if (auto AT = dyn_cast<clang::AutoType>(FPT->getReturnType()))
        if (!AT->isSugared()) { // not sugared if undeduced
            InstantiateFunctionDefinition(S, FuncInst);
            FPT = FuncInst->getType()->castAs<clang::FunctionProtoType>();
        }

    auto oldtf = (TypeFunction *) fd->type;
    fd->type = DeclMapper::FromType(mapper, ti->loc).fromTypeFunction(FPT, FuncInst);
    auto partial = doHeaderInstantiation(ti, sc, fd, tthis, fargs);
    fd->type = oldtf;
    fd = partial;

    return (MATCH)(match | (matchTiargs<<4));
}

bool TemplateDeclaration::isForeignInstance(::TemplateInstance *ti)
{
    return isCPP(ti) && static_cast<TemplateInstance*>(ti)->isForeignInst;
}

bool LangPlugin::isForeignInstance(::TemplateInstance *ti)
{
    return TemplateDeclaration::isForeignInstance(ti);
}

clang::RedeclarableTemplateDecl *getPrimaryTemplate(const clang::NamedDecl* TempOrSpec)
{
    if (auto RTD = dyn_cast<clang::RedeclarableTemplateDecl>(TempOrSpec))
        return const_cast<clang::RedeclarableTemplateDecl*>(RTD);

    if (auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(TempOrSpec))
        return CTSD->getSpecializedTemplate();

    if (auto VTSD = dyn_cast<clang::VarTemplateSpecializationDecl>(TempOrSpec))
        return VTSD->getSpecializedTemplate();

    if (auto FD = dyn_cast<clang::FunctionDecl>(TempOrSpec))
        return FD->getPrimaryTemplate();

    llvm_unreachable("Unhandled primary template");
}

clang::RedeclarableTemplateDecl *TemplateDeclaration::getPrimaryTemplate()
{
    return ::cpp::getPrimaryTemplate(TempOrSpec);
}

TemplateDeclaration* TemplateDeclaration::primaryTemplate()
{
    auto Prim = getPrimaryTemplate()->getCanonicalDecl();

    assert(parent->isScopeDsymbol());
    auto sym = dsymForDecl(static_cast<ScopeDsymbol*>(parent), Prim);
    assert(sym && sym->isTemplateDeclaration());

    return static_cast<TemplateDeclaration*>(sym);
}

::TemplateInstance* TemplateDeclaration::foreignInstance(::TemplateInstance* tithis,
                                                       Scope* sc)
{
    if (isForeignInstance(tithis))
        return nullptr;

    cpp::TemplateInstance* ti;
    if (isCPP(tithis))
        ti = static_cast<cpp::TemplateInstance *>(tithis);
    else
    {
        ti = new cpp::TemplateInstance(tithis->loc, tithis->name, tithis->tiargs); // HACK avoid arraySyntaxCopy, which is slow and would require many improvements to be reliable
        ti->semantictiargsdone = true;
        ti->tdtypes.setDim(tithis->tdtypes.dim);
        memcpy(ti->tdtypes.data, tithis->tdtypes.data, ti->tdtypes.dim * sizeof(void*));
    }

    if (!ti->Inst)
        ti->Inst = getClangInst(sc, ti);

    if (!ti->Inst)
    {
        assert(ti->errors);
        return ti;
    }

    correctTempDecl(ti);
    ti->isForeignInst = true;
    ti->havetempdecl = true;

    ti->correctTiargs();

    ti->semanticRun = PASSinit; // FIXME not necessary anymore
    ti->hash = 0;
    return ti;
}

TemplateInstUnion TemplateDeclaration::hasExistingClangInst(::TemplateInstance* ti)
{
    if (isCPP(ti))
        if (auto existingInst = static_cast<TemplateInstance*>(ti)->Inst)
            return existingInst;
    return TemplateInstUnion();
}

TemplateInstUnion TemplateDeclaration::getClangInst(Scope* sc, ::TemplateInstance* ti, Objects* tdtypes)
{
    if (auto existingInst = hasExistingClangInst(ti))
        return existingInst;

    if (isNonTemplateWrapper())
        return const_cast<clang::NamedDecl*>(TempOrSpec);

    auto Temp = const_cast<clang::RedeclarableTemplateDecl*>(
                                getDefinition(getPrimaryTemplate()));

    if (!tdtypes)
        tdtypes = &ti->tdtypes;

    DeclMapper mapper(sc->minst, sc->minst);
    ExprMapper expmap(mapper);

    clang::TemplateArgumentListInfo Args;
    fillTemplateArgumentListInfo(loc, sc, Args, tdtypes, Temp, mapper, expmap);

    auto& S = calypso.getSema();
    clang::TemplateName Name(Temp);

    if (isa<clang::ClassTemplateDecl>(Temp) ||
        isa<clang::TypeAliasTemplateDecl>(Temp))
    {
        auto Ty = S.CheckTemplateIdType(Name, Temp->getLocation(), Args); // NOTE: this also substitutes the argument types
                                    // to TemplateTypeParmType, which is needed for partial specializations to work
        if (!Ty.isNull()) {
            if (auto TST = Ty->getAs<clang::TemplateSpecializationType>())
                if (TST->isTypeAlias())
                    return TST;

            auto RT = Ty->getAs<clang::RecordType>();
            auto CTSD = cast<clang::ClassTemplateSpecializationDecl>(RT->getDecl());
            return CTSD;
        }
    }
    else if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(Temp))
    {
        assert(FuncTemp->getTemplatedDecl()->isDefined() ||
                FuncTemp->getInstantiatedFromMemberTemplate());

        if (auto FuncInst = instantiateFunctionDeclaration(Args, FuncTemp))
            return FuncInst;
    }
    else if (auto VarTemp = dyn_cast<clang::VarTemplateDecl>(Temp))
    {
        auto Result = S.CheckVarTemplateId(VarTemp, clang::SourceLocation(), Temp->getLocation(), Args);
        if (Result.isUsable())
        {
            auto VarInst = cast<clang::VarDecl>(Result.get());
            return VarInst;
        }
    }
    else
        assert(false);

    return TemplateInstUnion();
}

void TemplateDeclaration::correctTempDecl(TemplateInstance *ti)
{
    if (isNonTemplateWrapper())
    {
        ti->tempdecl = this;
        return;
    }

    const clang::Decl* RealTemp;

    if (auto SpecDecl = ti->Inst.dyn_cast<clang::NamedDecl*>())
        RealTemp = getSpecializedDeclOrExplicit(SpecDecl);
    else
    {
        auto TST = ti->Inst.get<const clang::TemplateSpecializationType*>();
        RealTemp = TST->getTemplateName().getAsTemplateDecl();
    }

    auto sym = dsymForDecl<DeclMapper::MapExplicitSpecs>(
                        static_cast<ScopeDsymbol*>(this->parent), RealTemp);
    assert(sym->isTemplateDeclaration());

    ti->tempdecl = static_cast<TemplateDeclaration*>(sym);

    assert(ti->tempdecl && isCPP(ti->tempdecl));
}

bool TemplateDeclaration::isNonTemplateWrapper()
{
    auto FD = dyn_cast<clang::FunctionDecl>(TempOrSpec);
    return FD && !FD->getPrimaryTemplate();
}

void TemplateDeclaration::accept(Visitor *v)
{
    v->visit(this);
}

/***********************/

TemplateInstance::TemplateInstance(Loc loc, Identifier* ident, Objects *tiargs)
{
    construct_TemplateInstance(this, loc, ident, tiargs);
}

TemplateInstance::TemplateInstance(Loc loc, ::TemplateDeclaration *td, Objects *tiargs)
{
    construct_TemplateInstance(this, loc, td, tiargs);
}

TemplateInstance::TemplateInstance(const TemplateInstance& o)
    : TemplateInstance(o.loc, o.name, o.tiargs)
{
    Inst = o.Inst;
    isForeignInst = o.isForeignInst;
    primTiargs = o.primTiargs;
}

Dsymbol *TemplateInstance::syntaxCopy(Dsymbol *s)
{
    if (!s)
        s = new cpp::TemplateInstance(*this);
    else
    {
        assert(s->isTemplateInstance() && isCPP(s));
        auto ti = static_cast<cpp::TemplateInstance*>(s);
        ti->Inst = Inst;
        ti->isForeignInst = isForeignInst;
        ti->primTiargs = primTiargs;
    }

    return ::TemplateInstance::syntaxCopy(s);
}

void TemplateInstance::addMember(Scope *sc, ScopeDsymbol *sds)
{
    cppAddMember(this, sc, sds);
}

// For (deco) mangling we need to retrieve the original arguments for the primary template.
Identifier *TemplateInstance::getIdent()
{
    auto a = tiargs;

    if (primTiargs)
        tiargs = primTiargs;
    auto result = ::TemplateInstance::getIdent();
    tiargs = a;

    return result;
}

size_t TemplateInstance::correspondingParamIdx(size_t argi)
{
    assert(tempdecl && tempdecl->isTemplateDeclaration() && isCPP(tempdecl));
    auto primtemp = static_cast<cpp::TemplateDeclaration*>(tempdecl)->primaryTemplate();
    return ::correspondingParamIdx(argi, primtemp, tiargs);
}

TemplateParameter* TemplateInstance::correspondingParam(size_t argi)
{
    assert(tempdecl && tempdecl->isTemplateDeclaration() && isCPP(tempdecl));
    auto primtemp = static_cast<cpp::TemplateDeclaration*>(tempdecl)->primaryTemplate();
    return (*primtemp->parameters)[::correspondingParamIdx(argi, primtemp, tiargs)];
}

Objects* TemplateInstance::tiargsForMangling()
{
    return primTiargs ? primTiargs : tiargs;
}

void TemplateInstance::correctTiargs()
{
    auto InstND = Inst.dyn_cast<clang::NamedDecl*>();

    assert(isCPP(tempdecl));
    auto TempOrSpec = static_cast<cpp::TemplateDeclaration*>(tempdecl)->TempOrSpec;

    // Correction is only needed for instances from partial specs
    if (auto PartialTempParams = getPartialTemplateSpecParameters(TempOrSpec))
    {
        auto Args = getTemplateInstantiationArgs(InstND)->asArray();

        DeclMapper mapper(minst, minst);

        assert(!primTiargs);
        primTiargs = tiargs;
        tiargs = DeclMapper::FromType(mapper, loc).fromTemplateArguments(Args.begin(), Args.end(),
                        PartialTempParams);
    }
}

void TemplateInstance::markInvalid()
{
    if (!errors)
    {
        error(loc, "error instantiating");
        if (tinst)
            tinst->printInstantiationTrace();
    }
    errors = true;

    if (!members)
        return;
    auto s = (*members)[0];
    s->errors = true;
}

const clang::NamedDecl* TemplateInstance::getInstantiatedTemplate()
{
    if (Inst.is<clang::NamedDecl*>())
        return cast<clang::NamedDecl>(
                        getSpecializedDeclOrExplicit(Inst.get<clang::NamedDecl*>()));
    else
        return Inst.get<const clang::TemplateSpecializationType*>()
                                                ->getTemplateName().getAsTemplateDecl();
}

// Debug functions
clang::NamedDecl* TemplateInstance::getInstND()
{
    return Inst.get<clang::NamedDecl*>();
}

const clang::TemplateSpecializationType* TemplateInstance::getInstTST()
{
    return Inst.get<const clang::TemplateSpecializationType*>();
}

}
