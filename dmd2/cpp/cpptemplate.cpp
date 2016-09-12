// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cpptemplate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "aggregate.h"
#include "enum.h"
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
    bool isAtLeastAsSpecializedAs(Sema &S,
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
    : ::TemplateDeclaration(loc, id, parameters, nullptr, decldefs)
{
    this->TempOrSpec = TempOrSpec;
    this->constraint = new NullExp(loc); // HACK to grant Sema another check e.g for templates with std::enable_if
}

TemplateDeclaration::TemplateDeclaration(const TemplateDeclaration &o)
    : TemplateDeclaration(o.loc, o.ident, o.parameters, o.members, o.TempOrSpec)
{
}

IMPLEMENT_syntaxCopy(TemplateDeclaration, TempOrSpec)

// HACK-ish unfortunately.. but arg deduction isn't trivial. Can't think of a simpler way.
// TODO: remove this, use cppdeclaration.cpp Referencer approach instead w/ the global scope
struct CppSymCollector
{
    Dsymbols *substsyms;
    CppSymCollector(Dsymbols *substsyms)
        : substsyms(substsyms) {}

    inline void addIfCPP(Dsymbol *s)
    {
        if (isCPP(s))
            substsyms->push(s);

        if (auto ti = s->parent->isTemplateInstance())
            if (isCPP(ti))
                collect(ti->tiargs);
    }

    void collect(Dsymbol *s)
    {
        addIfCPP(s);
    }

    void collect(Type *t)
    {
        switch (t->ty)
        {
            case Tstruct:
                addIfCPP(static_cast<TypeStruct*>(t)->sym);
                break;
            case Tclass:
                addIfCPP(static_cast<TypeClass*>(t)->sym);
                break;
            case Tenum:
                addIfCPP(static_cast<TypeEnum*>(t)->sym);
                break;
            case Tarray:
            case Tsarray:
            case Tpointer:
            case Treference:
                collect(static_cast<TypeNext*>(t)->next);
                break;
            case Tfunction:
            {
                auto tf = static_cast<TypeFunction*>(t);
                collect(tf->next);
                for (auto p: *tf->parameters)
                    collect(p->type);
                break;
            }
            case Tident:
            case Tinstance:
            default:
//                 ::warning(Loc(), "Collecting C++ symbols unhandled for type %s:\"%s\"",
//                           t->kind(), t->toChars());
                break;
        }
    }

    void collect(Expression *e)
    {
        // TODO DotIdExp ...
    }

    void collect(Tuple *tup)
    {
        collect(&tup->objects);
    }

    void collect(Objects *tiargs)
    {
        for (auto o: *tiargs)
        {
            Type *ta = isType(o);
            Expression *ea = isExpression(o);
            Dsymbol *sa = isDsymbol(o);
            Tuple *tupa = isTuple(o);

            if (ta) collect(ta);
            else if (ea) collect(ea);
            else if (sa) collect(sa);
            else { assert(tupa); collect(tupa); }
        }
    }
};

static Dsymbols *collectSymbols(Objects *tiargs)
{
    auto substsyms = new Dsymbols;
    CppSymCollector(substsyms).collect(tiargs);

    return substsyms;
}

static void fillTemplateArgumentListInfo(Loc loc, Scope *sc, clang::TemplateArgumentListInfo& Args,
                                Objects *tiargs, const clang::RedeclarableTemplateDecl *Temp,
                                TypeMapper& tymap, ExprMapper& expmap)
{
    auto& Context = calypso.getASTContext();

    SpecValue spec(tymap);
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
            auto T = tymap.toType(loc, ta, sc);
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

// Hijack the end of findTempDecl to only check forward refs for the unique candidate selected by Sema,
// which is done later in matchWithInstance.
bool TemplateDeclaration::checkTempDeclFwdRefs(Scope* sc, Dsymbol* tempdecl, ::TemplateInstance* ti)
{
    return true;
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

    TypeMapper tymap;
    ExprMapper expmap(tymap);
    tymap.addImplicitDecls = false;

    clang::TemplateArgumentListInfo Args;
    fillTemplateArgumentListInfo(ti->loc, sc, Args, dedtypes, Temp, tymap, expmap);

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

    if (!instSuccess)
        Diags.Reset();

    return instSuccess;
}

bool TemplateDeclaration::earlyFunctionValidityCheck(::TemplateInstance* ti, Scope* sc, Objects* dedtypes)
{
    return evaluateConstraint(ti, sc, nullptr, dedtypes, nullptr);
}

void TemplateDeclaration::prepareBestMatch(::TemplateInstance* ti, Scope* sc, Expressions* fargs)
{
    if (!ti->havetempdecl)
    {
        ti->havetempdecl = true;
        ti->tempdecl = primaryTemplate();
    }
}

MATCH TemplateDeclaration::matchWithInstance(Scope *sc, ::TemplateInstance *ti,
                                             Objects *dedtypes, Expressions *fargs, int flag)
{
    // Give only the primary template a chance to match
    // The "best matching" is done dy Sema, and then foreignInstance corrects ti->tempdecl
    if (!isa<clang::RedeclarableTemplateDecl>(TempOrSpec))
    {
        // Check if we're not calling matchWithInstance from a foreignInstance with the
        // tempdecl already determined, which might be a specialization.
        if (!isForeignInstance(ti))
            return MATCHnomatch;
    }

    // Do TemplateDeclaration::semantic() here instead of during findTempDecl
    // Qt's QTypeInfo has a massive amount of explicit specs that caused findTempDecl to break down,
    // so for class templates the preselection is done by Sema in prepareBestMatch and only the selected specialization gets semantic'd
    // NOTE: this might be an issue as well for function templates for some libraries.
    if (semanticRun == PASSinit)
    {
        if (scope)
        {
            // Try to fix forward reference. Ungag errors while doing so.
            Ungag ungag = ungagSpeculative();
            semantic(scope);
        }
        if (semanticRun == PASSinit)
        {
            ti->error("%s forward references template declaration %s", ti->toChars(), toChars());
        }
    }

    // TODO: clean everything up, make the flow "non-cpp" vs "cpp non-foreignInst" vs "cpp foreignInst" less insane!

    auto m = ::TemplateDeclaration::matchWithInstance(sc, ti, dedtypes, fargs, flag);

    if (m == MATCHnomatch || flag == 1) // 1 means it's from TemplateDeclaration::leastAsSpecialized
        return m;

    // Although the match result is ok, the types deducted by DMD may have been stripped of C++-specific info and end up wrong.
    // That's why we fix dedtypes by taking Inst's template instantiation args.

    auto Inst = hasExistingClangInst(ti);
    if (!Inst)
    {
        int minNumExplicitArgs = 0;
        for (auto param: *parameters)
        {
            minNumExplicitArgs++;
            if (param->hasDefaultArg())
                break;
        }
        auto tdtypes = TemplateInstance::arraySyntaxCopy(ti->tiargs);
        for (int i = (int)ti->tiargs->dim; i < minNumExplicitArgs; i++)
            tdtypes->push(objectSyntaxCopy((*dedtypes)[i])); // template arguments deduced from fargs or from the opCast type need to be pushed, and yes that means relying on DMD's template deduction..

        Inst = getClangInst(sc, ti, tdtypes);

        delete tdtypes;
    }

    TypeMapper tymap;
    tymap.addImplicitDecls = false;
    tymap.substsyms = collectSymbols(dedtypes);
    tymap.desugar = true;

    auto Temp = getPrimaryTemplate();
    auto InstArgs = (isForeignInstance(ti) ? getTemplateInstantiationArgs(Inst)
                                                            : getTemplateArgs(Inst))->asArray();

    auto cpptdtypes = TypeMapper::FromType(tymap, loc).fromTemplateArguments<true>(
                                            InstArgs.begin(), InstArgs.end(),
                                            Temp->getTemplateParameters());

    SpecValue spec(tymap);
    if (Inst.is<clang::NamedDecl*>())
        getIdentifier(Inst.get<clang::NamedDecl*>(), &spec, true);
    if (spec)
        cpptdtypes->shift(spec.toTemplateArg(loc));

    if (isVariadic() && cpptdtypes->dim == dedtypes->dim - 1)
        cpptdtypes->push(new Tuple);
    if (cpptdtypes->dim == dedtypes->dim + 1) { // might happen for explicit spec where the pack has zero argument. This feels like a hack, needs cleanup.
        auto tup = isTuple(cpptdtypes->data[0]);
        assert(tup && tup->objects.dim == 0);
        cpptdtypes->dim--;
    }
    assert(cpptdtypes->dim == dedtypes->dim);

    for (unsigned i = 0; i < cpptdtypes->dim; i++)
        (*dedtypes)[i] = (*cpptdtypes)[i];

    // Assuming that all symbols from the instantiating scope were collected and substitued, the remaining TypeQualified&co need
    // to be semantic'd in the template parameter scope.
    ScopeDsymbol *paramsym = new ScopeDsymbol();
    paramsym->parent = scope->parent;
    Scope *paramscope = scope->push(paramsym);
    paramscope->tinst = ti;
    paramscope->callsc = sc;
    paramscope->stc = 0;

    for (size_t i = 0; i < dedtypes->dim; i++)
    {
        TemplateParameter *tp = (*parameters)[i];
        RootObject *o = (*dedtypes)[i];

        if (!declareParameter(paramscope, tp, o))
            assert(false);
    }

    ::TemplateInstance::semanticTiargs(ti->loc, paramscope, dedtypes, 4);
    return m;
}

// In some cases DMD's leastAsSpecialized isn't enough, for ex. to disambiguate "this(_Tp)(vector!(vector!_Tp))" over "this(_Tp)(vector!_Tp)"
// (and in that particular case, we can't even supply explicit template arguments to the ctor..)
// Fortunately Sema has a method helping refine DMD's result
MATCH TemplateDeclaration::leastAsSpecialized(Scope* sc, ::TemplateDeclaration* td2, Expressions* fargs)
{
    auto Prim = getPrimaryTemplate();

    auto m = ::TemplateDeclaration::leastAsSpecialized(sc, td2, fargs);
    if (m == MATCHnomatch || !isCPP(td2) || !isa<clang::FunctionTemplateDecl>(Prim))
        return m;

    auto& Sema = calypso.getSema();

    auto c_td2 = static_cast<cpp::TemplateDeclaration*>(td2);
    auto FuncTemp1 = cast<clang::FunctionTemplateDecl>(Prim);
    auto FuncTemp2 = cast<clang::FunctionTemplateDecl>(c_td2->getPrimaryTemplate());

    const unsigned TDF_IgnoreQualifiers = 0x02;
    auto Better1 = clang::isAtLeastAsSpecializedAs(Sema, clang::SourceLocation(), FuncTemp1, FuncTemp2,
                                isa<clang::CXXConversionDecl>(FuncTemp1->getTemplatedDecl())? clang::TPOC_Conversion : clang::TPOC_Call,
                                fargs->dim, TDF_IgnoreQualifiers);
    if (!Better1)
        m = MATCHnomatch;

    return m;
}

bool TemplateDeclaration::isForeignInstance(::TemplateInstance *ti)
{
    return isCPP(ti) && static_cast<TemplateInstance*>(ti)->isForeignInst;
}

clang::RedeclarableTemplateDecl *getPrimaryTemplate(const clang::NamedDecl* TempOrSpec)
{
    if (auto RTD = dyn_cast<clang::RedeclarableTemplateDecl>(TempOrSpec))
        return const_cast<clang::RedeclarableTemplateDecl*>(RTD);

    if (auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(TempOrSpec))
        return CTSD->getSpecializedTemplate();

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

    ::TemplateDeclaration *td = this;
    if (td->overroot)
        td = td->overroot;

    // Find and set the correct tempdecl of the instance
    for (; td; td = td->overnext)
    {
        auto c_td = static_cast<cpp::TemplateDeclaration*>(td);
        if (c_td->TempOrSpec->getCanonicalDecl() == Prim)
            return c_td;
    }

    llvm_unreachable("No primary template found");
    return nullptr;
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
        ti = new cpp::TemplateInstance(tithis->loc, tithis->name);
        tithis->syntaxCopy(ti);

        ti->tdtypes.setDim(tithis->tdtypes.dim);
        for (size_t i = 0; i < tithis->tdtypes.dim; i++)
            ti->tdtypes[i] = objectSyntaxCopy(tithis->tdtypes[i]);

        ti->semantictiargsdone = false; // redo semanticTiargs because most TypeXXX::syntaxCopy throw away decos
        ti->semanticTiargs(sc);
    }

    if (!ti->Inst)
        ti->Inst = getClangInst(sc, ti);

    if (!ti->Inst)
    {
        assert(ti->errors);
        return ti;
    }

    makeForeignInstance(ti);

    ti->semanticRun = PASSinit; // WARNING: may disrupt something?
    ti->hash = 0;
    ti->semantic(sc);
    return ti;
}

void TemplateDeclaration::makeForeignInstance(TemplateInstance* ti)
{
    ti->completeInst();

    correctTempDecl(ti);
    ti->isForeignInst = true;
    ti->havetempdecl = true;

    ti->correctTiargs();
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

    auto& S = calypso.getSema();

    TypeMapper tymap;
    ExprMapper expmap(tymap);
    tymap.addImplicitDecls = false;
    tymap.cppPrefix = false;

    auto Temp = const_cast<clang::RedeclarableTemplateDecl*>
                                (getDefinition(getPrimaryTemplate(), false));

    if (!tdtypes)
        tdtypes = &ti->tdtypes;

    clang::TemplateArgumentListInfo Args;
    fillTemplateArgumentListInfo(loc, sc, Args, tdtypes, Temp, tymap, expmap);

    clang::TemplateName Name(Temp);

    if (isa<clang::ClassTemplateDecl>(Temp) ||
            isa<clang::TypeAliasTemplateDecl>(Temp))
    {
        auto Ty = S.CheckTemplateIdType(Name, Temp->getLocation(), Args); // NOTE: this also substitutes the argument types
                                            // to TemplateTypeParmType, which is needed for partial specializations to work.

        if (auto TST = Ty->getAs<clang::TemplateSpecializationType>())
            if (TST->isTypeAlias())
                return TST;

        auto RT = Ty->getAs<clang::RecordType>();
        auto CTSD = cast<clang::ClassTemplateSpecializationDecl>(RT->getDecl());
        return CTSD;
    }
    else if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(Temp))
    {
        assert(FuncTemp->getTemplatedDecl()->isDefined() ||
                FuncTemp->getInstantiatedFromMemberTemplate());

        auto FuncInst = instantiateFunctionDeclaration(Args, FuncTemp);
        assert(FuncInst);

        // Then the definition
        S.InstantiateFunctionDefinition(Temp->getLocation(), FuncInst, true);

        return FuncInst;
    }
    else
        assert(false);

    return TemplateInstUnion();
}

::TemplateDeclaration* TemplateDeclaration::getCorrespondingTempDecl(TemplateInstUnion Inst)
{
    const clang::Decl* RealTemp;

    if (auto SpecDecl = Inst.dyn_cast<clang::NamedDecl*>())
    {
        auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(SpecDecl);
        auto FuncSpec = dyn_cast<clang::FunctionDecl>(SpecDecl);

        const clang::Decl *Spec = ClassSpec;
        if (FuncSpec)
            Spec = FuncSpec;

        RealTemp = getSpecializedDeclOrExplicit(Spec);
    }
    else
    {
        auto TST = Inst.get<const clang::TemplateSpecializationType*>();
        RealTemp = TST->getTemplateName().getAsTemplateDecl();
    }

    RealTemp = RealTemp->getCanonicalDecl();

    ::TemplateDeclaration *td = this;
    if (td->overroot)
        td = td->overroot;

    // Find and set the correct tempdecl of the instance
    for (; td; td = td->overnext)
    {
        if (!isCPP(td))
        {
            ::warning(td->loc, "Unexpected non C++ template declaration");
            continue;
        }

        auto c_td = static_cast<cpp::TemplateDeclaration*>(td);
        if (c_td->TempOrSpec->getCanonicalDecl() == RealTemp)
            return c_td;
    }

    return nullptr;
}

void TemplateDeclaration::correctTempDecl(TemplateInstance *ti)
{
    ti->tempdecl = getCorrespondingTempDecl(ti->Inst);

    assert(ti->tempdecl && isCPP(ti->tempdecl)/* &&
            static_cast<cpp::TemplateDeclaration*>(ti->tempdecl)
                            ->TempOrSpec->getCanonicalDecl() == RealTemp*/);
}

TemplateInstance::TemplateInstance(Loc loc, Identifier* temp_id)
    : ::TemplateInstance(loc, temp_id)
{
}

TemplateInstance::TemplateInstance(Loc loc, ::TemplateDeclaration *tempdecl, Objects *tiargs)
    : ::TemplateInstance(loc, tempdecl, tiargs)
{
}

TemplateInstance::TemplateInstance(const TemplateInstance& o)
    : TemplateInstance(o.loc, o.name)
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

bool TemplateInstance::semanticTiargs(Scope* sc)
{
    auto result = ::TemplateInstance::semanticTiargs(sc);

    if (result && Inst && !isForeignInst)
    {
        // We query the evaluated arguments by Sema and possibly fix the tiargs semantic'd by DMD since there might be slight differences.
        // See for example Qt's is_unsigned<Qt::KeyboardModifiers>, the value will be different because of T(0) < T(-1) which doesn't lead to the expected result in DMD.
        // The issue wasn't apparent before because DMD and C++'s expression evaluation match in more than 99% of cases.
        // And this could become redundant if clang::Expr gets preserved and sent back to Clang for evaluation in the future.

        auto Temp = getPrimaryTemplate(getInstantiatedTemplate());

        TypeMapper tymap;
        ExprMapper expmap(tymap);
        tymap.addImplicitDecls = false;
        tymap.cppPrefix = false;

        auto Args = getTemplateArgs(Inst)->asArray();
        auto Arg = Args.begin();
        auto Param = Temp->getTemplateParameters()->begin();

        SpecValue spec(tymap);
        getIdentifierOrNull(Temp, &spec);

        auto sc2 = globalScope(sc->instantiatingModule());

        for (size_t i = spec ? 1 : 0; i < tiargs->dim; Arg++, Param++)
        {
            auto NTTPD = dyn_cast<clang::NonTypeTemplateParmDecl>(*Param);
            if (!NTTPD && !isTemplateParameterPack(*Param)) {
                i++;
                continue;
            }

            auto a = TypeMapper::FromType(tymap, loc).fromTemplateArgument(Arg, *Param);
            for (auto arg: *a) {
                if (auto e = isExpression(arg)) {
                    assert(isExpression((*tiargs)[i]));
                    (*tiargs)[i] = e->semantic(sc2);
                }
                i++;
            }
        }
    }

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

bool TemplateInstance::completeInst()
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.getSema();
    auto& Diags = calypso.getDiagnostics();

    auto CTSD = dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(
                    Inst.dyn_cast<clang::NamedDecl*>());

    if (CTSD && !CTSD->hasDefinition()/* &&
        CTSD->getSpecializedTemplate()->getTemplatedDecl()->hasDefinition()*/) // unused forward template specialization decls will exist but as empty aggregates
    {
        auto Ty = Context.getRecordType(CTSD);

        if (S.RequireCompleteType(CTSD->getLocation(), Ty, clang::diag::err_incomplete_type))
            Diags.Reset();
    }

    return true;
}

void TemplateInstance::correctTiargs()
{
    auto CTSD = dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(
                    Inst.dyn_cast<clang::NamedDecl*>());

    if (!CTSD)
        return;

    assert(isCPP(tempdecl));
    auto TempOrSpec = static_cast<cpp::TemplateDeclaration*>(tempdecl)->TempOrSpec;

    // Correction is only needed for instances from partial specs
    if (auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(TempOrSpec))
    {
        auto Args = CTSD->getTemplateInstantiationArgs().asArray();
            // NOTE: The issue with getTemplateInstantiationArgs() is that the arguments are already substitued.
            // Since the deduced arguments are contained in one form or another in the original args,
            // the trick is to reference C++ symbols inside the original args and tell TypeMapper to use them.

        TypeMapper tymap;
        tymap.addImplicitDecls = false;
        tymap.substsyms = collectSymbols(tiargs);

        primTiargs = tiargs;
        tiargs = TypeMapper::FromType(tymap, loc).fromTemplateArguments(Args.begin(), Args.end(),
                        Partial->getTemplateParameters());
        semantictiargsdone = false;
    }
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