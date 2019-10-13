// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cpptemplate.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/cppmodule.h"
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

const clang::TemplateParameterList *getTemplateParameters(const clang::Decl *D)
{
    if (auto TPL = getPartialTemplateSpecParameters(D))
        return TPL;
    else if (auto ClassTemp = dyn_cast<clang::ClassTemplateDecl>(D))
        return ClassTemp->getTemplateParameters();
    else if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
        return FuncTemp->getTemplateParameters();
    else if (auto VarTemp = dyn_cast<clang::VarTemplateDecl>(D))
        return VarTemp->getTemplateParameters();

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

// TODO: remove prepareBestMatch and firstTempDecl? they shouldn't be necessary if the overroot is the primary template or the dispatching template
void TemplateDeclaration::prepareBestMatch(::TemplateInstance* ti, Scope* sc, Expressions* fargs)
{
    if (!isa<clang::FunctionTemplateDecl>(TempOrSpec) &&
        !isa<clang::FunctionDecl>(TempOrSpec) &&
        !ti->havetempdecl)
    {
        ti->havetempdecl = true;
        ti->tempdecl = primaryTemplate();
    }
}

MATCH TemplateDeclaration::matchWithInstance(Scope *sc, ::TemplateInstance *ti,
                                             Objects *dedtypes, Expressions *fargs, int flag)
{
    if (isDispatching)
        return MATCHnomatch;

    // Give only the primary template a chance to match if it's not the special case of
    // a wrapped non-templated overloaded operator
    // The "best matching" is done dy Sema, and then foreignInstance corrects ti->tempdecl
    assert(isa<clang::RedeclarableTemplateDecl>(TempOrSpec) || isNonTemplateWrapper());

    TemplateInstUnion Inst;
    assert(!isCPP(ti));

    MATCH m = MATCHexact;

    if (isa<clang::FunctionTemplateDecl>(TempOrSpec))
        m = functionTemplateMatch(sc, ti, fargs, Inst);
    else {
        Inst = getClangInst(sc, ti->tiargs);
        if (!Inst)
            m = MATCHnomatch;
    }

    if (m == MATCHnomatch || flag == 1) // 1 means it's from TemplateDeclaration::leastAsSpecialized
        return m;

    std::unique_ptr<Objects> cpptdtypes(tdtypesFromInst(sc, Inst));
    assert(cpptdtypes->dim == dedtypes->dim);
    memcpy(dedtypes->tdata(), cpptdtypes->tdata(), dedtypes->dim * sizeof(void*));

    return m;
}

Objects* TemplateDeclaration::tdtypesFromInst(Scope* sc, TemplateInstUnion Inst)
{
    auto minst = sc ? sc->minst : nullptr;
    DeclMapper mapper(minst, minst);

    Objects* cpptdtypes;

    if (!isNonTemplateWrapper())
    {
        auto InstArgs = getTemplateArgs(Inst);
        auto ParamList = getPrimaryTemplate()->getTemplateParameters();

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
    if (!isNonTemplateWrapper())
        return nullptr;

    return DeclMapper(this).dsymForDecl(TempOrSpec);
}

Dsymbols* TemplateDeclaration::copySyntaxTree(::TemplateInstance *ti)
{
    assert(isForeignInstance(ti));
    auto c_ti = static_cast<cpp::TemplateInstance*>(ti);

    assert(!ti->members); // members were already set during decl mapping??

    auto Inst = c_ti->Inst.get<clang::NamedDecl*>();
    Dsymbol* inst = nullptr;

    if (auto sym = wrappedNonTemplateSymbol())
    {
        inst = new_AliasDeclaration(loc, sym->ident, sym);
        inst->semanticRun = PASSsemantic3done;
    }
    else
    {
        DeclMapper(ti->minst, ti->minst).VisitDecl(Inst);
        inst = Inst->d->sym.getPointer();
    }

    auto a = new Dsymbols;
    if (inst)
    {
        a->push(inst);
        c_ti->aliasdecl = inst;
    }
    return a;
}

static MATCH functionTemplateMatchNonTemplated(TemplateDeclaration* tempdecl, Scope *sc,
                            ::TemplateInstance *ti, Expressions *fargs, TemplateInstUnion& Inst)
{
    // If more arguments than parameters, no match
    if (ti->tiargs->dim > tempdecl->parameters->dim)
        return MATCHnomatch;

    Objects dedtypes;
    dedtypes.setDim(1);
    assert(dedtypes.dim == tempdecl->parameters->dim);

    size_t argi = 0;
    auto tp = (*tempdecl->parameters)[0];

    MATCH m = matchArg(tp, ti->loc, sc /* HACK-ish, everything is already semantic'd */,
                       ti->tiargs, 0, &argi /* CALYPSO */,
                       tempdecl->parameters, &dedtypes, nullptr);
    if (m)
        Inst = const_cast<clang::NamedDecl*>(tempdecl->TempOrSpec);

    return m;
}

MATCH TemplateDeclaration::functionTemplateMatch(Scope *sc, ::TemplateInstance *ti,
                                                 Expressions *fargs, TemplateInstUnion& Inst)
{
    if (isNonTemplateWrapper())
        return functionTemplateMatchNonTemplated(this, sc, ti, fargs, Inst);

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
    {
        SpecValue spec(mapper);
        getIdentifierOrNull(FunctionTemplate, &spec);

        if (spec)
        {
            Objects dedtypes;
            dedtypes.setDim(1);
            assert(parameters->dim >= 1);

            auto tp = (*parameters)[0];
            size_t argi = 0;
            MATCH m = matchArg(tp, ti->loc, sc, ti->tiargs, 0, &argi, parameters, &dedtypes, nullptr);

            if (m == MATCHnomatch)
                return MATCHnomatch;
        }

        fillTemplateArgumentListInfo(ti->loc, /*sc=*/ nullptr, ExplicitTemplateArgs, ti->tiargs,
                                     FunctionTemplate, mapper, expmap);
    }

    clang::FunctionDecl *Specialization;

    if (isConversion)
    {
        assert(ti->tiargs && ti->tiargs->dim >= 1);
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

// MATCH TemplateDeclaration::dispatchMatch(Scope *sc, ::TemplateInstance *ti,
//                                          Expressions *fargs, TemplateInstUnion& Inst)
// {
//     if (!ti->tiargs || ti->tiargs->dim < 1)
//         return MATCHnomatch; // needs at least the spec arg
//
//     auto& Context = calypso.getASTContext();
//     auto& S = calypso.getSema();
//
//     clang::SourceLocation Loc;
//     clang::sema::TemplateDeductionInfo DedInfo(Loc);
//
//     bool isConversion = ident == Id::_cast;
//
//     DeclMapper mapper(ti->minst, ti->minst);
//
//     auto specArg = (*ti->tiargs)[0];
//
//     Expression* specExp = nullptr;
//     if (specArg->dyncast() == DYNCAST_EXPRESSION)
//         specExp = static_cast<Expression*>(specArg);
//
//     if (!specExp || specExp->op != TOKstring)
//         return MATCHnomatch;
//
//     const char* op = static_cast<StringExp*>(specExp)->toStringz();
//     auto Op = toOverloadedOperator(ident, op);
//
//     if (Op == clang::OO_Spaceship)
//         return MATCHnomatch; // unrecognized spec arg
//
//     if (fargs)
//     {
//         auto parg = (*fargs)[0]->type;
//         if (auto agg = getAggregateSym(parg))
//             if (auto mod = agg->toParent()->isModule())
//             {
//                 assert(isCPP(mod));
//                 auto c_mod = static_cast<cpp::Module*>(mod);
//                 DeclMapper mapper(c_mod);
//
//                 c_mod->searchNonMemberOverloadedOperators(Op);
//                 auto& OOs = c_mod->nonMemberOverloadedOperators[Op].OOs;
//
//                 if (OOs.empty())
//                     return MATCHnomatch;
//
//                 Dsymbol* overroot = nullptr;
//                 for (auto OO: c_mod->nonMemberOverloadedOperators[Op].OOs)
//                 {
//                     auto s = mapper.templateForDecl(OO);
//                     if (!overroot)
//                         overroot = s;
//                 }
//                 assert(overroot && overroot->isTemplateDeclaration());
//             }
//     }
//
//     clang::FunctionDecl *Specialization;
//
// //     if (isConversion)
// //     {
// //         assert(ti->tiargs && ti->tiargs->dim >= 1);
// //         Type* to = isType((*ti->tiargs)[0]);
// //         clang::QualType To = mapper.toType(ti->loc, to, /*sc=*/ nullptr);
// //
// //         clang::CXXConversionDecl *Conversion;
// //
// //         if (S.DeduceTemplateArguments(const_cast<clang::FunctionTemplateDecl*>(FunctionTemplate),
// //                     To, Conversion, DedInfo))
// //             return MATCHnomatch;
// //         Specialization = Conversion;
// //     }
// //     else  if (fargs) {
// //         llvm::SmallVector<clang::Expr*, 4> Args;
// //         for (auto farg: *fargs) {
// //             // toType won't take dynamic arrays, but anticipate implicit conversions
// //             auto argty = farg->type;
// //             if (argty->ty == Tarray)
// //                 argty = argty->nextOf()->pointerTo();
// //
// //             auto ArgTy = mapper.toType(ti->loc, argty, /*sc=*/ nullptr);
// //             auto DummyExpr = new (Context) clang::OpaqueValueExpr(Loc, ArgTy,
// //                                         farg->isLvalue() ? clang::VK_LValue : clang::VK_RValue);
// //             Args.push_back(DummyExpr);
// //         }
// //
// //         if (S.DeduceTemplateArguments(const_cast<clang::FunctionTemplateDecl*>(FunctionTemplate),
// //                     &ExplicitTemplateArgs, Args, Specialization, DedInfo, false,
// //                     [](llvm::ArrayRef<clang::QualType>){ return false; }))
// //             return MATCHnomatch;
// //     } else if (S.DeduceTemplateArguments(const_cast<clang::FunctionTemplateDecl*>(FunctionTemplate),
// //                     &ExplicitTemplateArgs, Specialization, DedInfo))
// //         return MATCHnomatch;
//
//     Inst = Specialization;
//     return MATCHexact;
// }

Dsymbol *LangPlugin::dispatchFuncCall(const Loc &loc, Scope *sc, Dsymbol *s,
        Objects *tiargs, Type *tthis, Expressions *fargs)
{
    if (!tiargs || tiargs->dim < 1)
        return nullptr; // needs at least the spec arg

    auto td = s->isTemplateDeclaration();
    if (!td)
        return nullptr;

    auto c_td = static_cast<cpp::TemplateDeclaration*>(td);
    if (!c_td->isDispatching)
        return nullptr;

//     auto& Context = calypso.getASTContext();
//     auto& S = calypso.getSema();

    clang::SourceLocation Loc;
    clang::sema::TemplateDeductionInfo DedInfo(Loc);

    bool isBinary = s->ident == Id::opBinary || s->ident == Id::opBinaryRight || s->ident == Id::opOpAssign;
// //     bool isConversion = s->ident == Id::_cast;

    auto specArg = (*tiargs)[0];

    Expression* specExp = nullptr;
    if (specArg->dyncast() == DYNCAST_EXPRESSION)
        specExp = static_cast<Expression*>(specArg);
    if (!specExp || specExp->op != TOKstring)
        return nullptr;

    const char* op = static_cast<StringExp*>(specExp)->toStringz();
    auto Op = toOverloadedOperator(s->ident, op);

    if (Op == clang::OO_Spaceship)
        return nullptr; // unrecognized spec arg

    assert(c_td->parent);
    auto agg = c_td->parent->isAggregateDeclaration();
        // case #1: parent is an aggregate

    cpp::Module* c_mod = nullptr;
    if (!agg)
    {
        auto mod = c_td->parent->isModule();
        assert(mod && isCPP(mod));

        if (!isFullNamespaceModule(mod))
            // case #2: parent is a cpp::Module for an aggregate
            c_mod = static_cast<cpp::Module*>(mod);
        else
        {
            // case #3: parent is a full namespace module
            if (fargs)
            {
                auto parg = (*fargs)[0]->type;

                if (!agg)
                    agg = getAggregateSym(parg);

                if (!agg && isBinary && fargs->dim >= 2)
                {
                    parg = (*fargs)[1]->type;
                    agg = getAggregateSym(parg);
                }

                if (!agg)
                    return nullptr;

                mod = agg->toParent()->isModule();
                if (mod)
                {
                    assert(mod && isCPP(mod) && !isFullNamespaceModule(mod));
                    c_mod = static_cast<cpp::Module*>(mod);
                }
            }
        }
    }

    if (c_mod)
    { // case #2 or #3
        DeclMapper mapper(c_mod);

        c_mod->searchNonMemberOverloadedOperators(Op);
        auto& OOs = c_mod->nonMemberOverloadedOperators[Op].OOs;

        if (OOs.empty())
            return nullptr;

        Dsymbol* sym = nullptr;
        for (auto OO: c_mod->nonMemberOverloadedOperators[Op].OOs)
        {
            auto s = mapper.templateForDecl(OO);
            if (!sym)
                sym = s;
        }
        assert(sym && sym->isTemplateDeclaration());

        if (auto overroot = static_cast<TemplateDeclaration*>(sym)->overroot)
            return overroot;
        else
            return sym;
    }
    else if (agg)
    { // case #1
        auto& DeclarationNames = calypso.getASTContext().DeclarationNames;
        auto Name = DeclarationNames.getCXXOperatorName(Op);

        auto RD = getRecordDecl(agg);

        for (auto Match: RD->lookup(Name))
            templateForDecl(agg, Match);

        return s; // the dispatching template has to be the overroot
    }

    return nullptr;
}

MATCH TemplateDeclaration::deduceFunctionTemplateMatch(::TemplateInstance *ti, Scope *sc,
                            ::FuncDeclaration *&fd, Type *tthis, Expressions *fargs)
{
    if (isDispatching)
        return MATCHnomatch;

    TemplateInstUnion Inst;

    if (functionTemplateMatch(sc, ti, fargs, Inst) != MATCHexact)
        return MATCHnomatch;

    MATCH matchCall = MATCHexact,
          matchTiargs = MATCHexact;

    auto dedtypes = tdtypesFromInst(sc, Inst);
    ti->tdtypes.dim = dedtypes->dim;
    ti->tdtypes.data = dedtypes->data;
    ti->tiargs = &ti->tdtypes;

    // HACK
    auto FuncInst = cast<clang::FunctionDecl>(Inst.get<clang::NamedDecl*>());
    auto FPT = FuncInst->getType()->castAs<clang::FunctionProtoType>();

    if (auto AT = dyn_cast<clang::AutoType>(FPT->getReturnType()))
        if (!AT->isSugared())
        { // not sugared if undeduced
            auto& S = calypso.getSema();

            InstantiateFunctionDefinition(S, FuncInst);
            FPT = FuncInst->getType()->castAs<clang::FunctionProtoType>();
        }

    DeclMapper mapper(this);
    auto tf = DeclMapper::FromType(mapper, ti->loc).fromTypeFunction(FPT, FuncInst);
    assert(tf->deco);
    fd->type = tf;

    matchCall = tf->callMatch(tthis, fargs, 0);
    return (MATCH)(matchCall | (matchTiargs<<4));
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

    return nullptr;
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
    assert(!isCPP(tithis));

    cpp::TemplateInstance* ti = nullptr;

    auto Inst = getClangInst(sc, &tithis->tdtypes);
    assert(Inst);

    if (auto D = Inst.dyn_cast<clang::NamedDecl*>())
        if (D->d && !isNonTemplateWrapper())
        {  // a TemplateInstance already exists (it was speculative)
            auto sym = D->d->sym.getPointer();
            assert(sym && sym->parent && sym->parent->isTemplateInstance() && isCPP(sym->parent));

            ti = static_cast<cpp::TemplateInstance*>(sym->parent);
            assert(!ti->minst);

            ti->minst = tithis->minst;
            ti->tinst = tithis->tinst;
        }

    if (!ti)
    {
        ti = new cpp::TemplateInstance(tithis->loc, this, tithis->tiargs); // HACK avoid arraySyntaxCopy, which is slow and would require many improvements to be reliable
        ti->isForeignInst = true;

        ti->Inst = Inst;

        auto tdtypes = tdtypesFromInst(sc, ti->Inst);
        ti->tdtypes.dim = tdtypes->dim;
        ti->tdtypes.data = tdtypes->data;

        ti->tinst = tithis->tinst;
        ti->minst = tithis->minst;

        assert(!findExistingInstance(ti, nullptr));

        ti->inst = ti;
        ti->parent = ti->enclosing ? ti->enclosing : parent; // NOTE: .enclosing is non-null only if one of the template args refer to a local symbol

        ti->members = copySyntaxTree(ti);
        ti->symtab = new_DsymbolTable();

        for (auto s: *ti->members)
        {
            assert(!s->parent);
            s->addMember(nullptr, ti);
        }

        ti->semanticRun = PASSsemantic3done;
    }

    if (ti->minst)
    {
        // only attach the instance to the primary template if it's not speculative
        // thus, templateInstanceSemantic() is forced to go back to foreignInstance()
        // until ti isn't speculative anymore
        ti->appendToModuleMember();
        addInstance(ti);
    }

    return ti;
}

TemplateInstUnion TemplateDeclaration::hasExistingClangInst(::TemplateInstance* ti)
{
    if (isCPP(ti))
        if (auto existingInst = static_cast<TemplateInstance*>(ti)->Inst)
            return existingInst;
    return TemplateInstUnion();
}

TemplateInstUnion TemplateDeclaration::getClangInst(Scope* sc, Objects* tdtypes)
{
    if (isNonTemplateWrapper())
        return const_cast<clang::NamedDecl*>(TempOrSpec);

    auto Temp = const_cast<clang::RedeclarableTemplateDecl*>(
                                getDefinition(getPrimaryTemplate()));

    DeclMapper mapper(this);
    ExprMapper expmap(mapper);

    clang::TemplateArgumentListInfo Args;
    fillTemplateArgumentListInfo(loc, sc, Args, tdtypes, Temp, mapper, expmap);

    auto& S = calypso.getSema();
    clang::TemplateName Name(Temp);

    clang::NamedDecl* InstDecl = nullptr;

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
            InstDecl = cast<clang::ClassTemplateSpecializationDecl>(RT->getDecl());
        }
    }
    else if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(Temp))
    {
        assert(FuncTemp->getTemplatedDecl()->isDefined() ||
                FuncTemp->getInstantiatedFromMemberTemplate());

        if (auto FuncInst = instantiateFunctionDeclaration(Args, FuncTemp))
            InstDecl = FuncInst;
    }
    else if (auto VarTemp = dyn_cast<clang::VarTemplateDecl>(Temp))
    {
        auto Result = S.CheckVarTemplateId(VarTemp, clang::SourceLocation(), Temp->getLocation(), Args);
        if (Result.isUsable())
            InstDecl = cast<clang::VarDecl>(Result.get());
    }
    else
        assert(false);

    if (InstDecl)
        return cast<clang::NamedDecl>(
                        const_cast<clang::Decl*>(getCanonicalDecl(InstDecl)));
    else
        return TemplateInstUnion();
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
    }

    return ::TemplateInstance::syntaxCopy(s);
}

Dsymbols* TemplateInstance::appendToModuleMember()
{
    if (minst)
    {
        minst->members->push(this);
        memberOf = minst;

        return minst->members;
    }

    // NOTE: DMD's appendToModuleMember() is weird, it attaches the instance to
    //  the root/codegen module importing the module containing the tempdecl,
    //  why is that even needed?
    //  and in our case attaching to the "wrong" module breaks the needsGen logic

    return nullptr;
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
