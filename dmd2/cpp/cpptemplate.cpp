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
#include "clang/Sema/Template.h"
#include "clang/Sema/TemplateDeduction.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
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

    void collect(Objects *tiargs)
    {
        for (auto o: *tiargs)
        {
            Type *ta = isType(o);
            Expression *ea = isExpression(o);
            Dsymbol *sa = isDsymbol(o);

            if (ta) collect(ta);
            else if (ea) collect(ea);
            else { assert(sa); collect(sa); }
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

    // See translateTemplateArgument() in SemaTemplate.cpp
    for (unsigned i = 0; i < tiargs->dim; i++)
    {
        if (i == 0 && spec)
            continue; // skip the first parameter of opUnary/opBinary/opOpAssign/...

        auto o = (*tiargs)[i];

        Type *ta = isType(o);
        Expression *ea = isExpression(o);
        Dsymbol *sa = isDsymbol(o);

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
        else
            assert(false && "unhandled template arg C++ -> D conversion");
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

clang::FunctionDecl *instantiateFunctionDeclaration(clang::TemplateArgumentListInfo& Args,
                                clang::FunctionTemplateDecl *FuncTemp)
{
    auto& S = calypso.getSema();

    // Converts TemplateArgumentListInfo to something suitable for TemplateArgumentList
    llvm::SmallVector<clang::TemplateArgument, 4> Converted;
    if (S.CheckTemplateArgumentList(FuncTemp, FuncTemp->getLocation(), Args,
                                    false, Converted))
        assert(false && "CheckTemplateArgumentList failed for function template");

    clang::TemplateArgumentList ArgList(clang::TemplateArgumentList::OnStack,
                            Converted.data(), Converted.size());
    clang::MultiLevelTemplateArgumentList MultiList(ArgList);

    // Instantiate the declaration
    clang::Sema::InstantiatingTemplate Instantiating(S,
                    FuncTemp->getLocation(), FuncTemp->getTemplatedDecl());
    if (Instantiating.isInvalid())
        assert(false && "InstantiatingTemplate is invalid");

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
bool TemplateDeclaration::evaluateConstraint(::TemplateInstance* ti, Scope* sc, Scope* paramscope,
                                            Objects* dedtypes, ::FuncDeclaration* fd)
{
    auto& Diags = calypso.getDiagnostics();
    auto Temp = getPrimaryTemplate();

    TypeMapper tymap;
    ExprMapper expmap(tymap);
    tymap.addImplicitDecls = false;

    if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(Temp))
    {
        clang::TemplateArgumentListInfo Args;
        fillTemplateArgumentListInfo(ti->loc, sc, Args, dedtypes, Temp, tymap, expmap);

        if (!instantiateFunctionDeclaration(Args, cast<clang::FunctionTemplateDecl>(FuncTemp)))
        {
            Diags.Reset();
            return false;
        }
    }
    return true;
}

void TemplateDeclaration::prepareBestMatch(::TemplateInstance* ti, Scope* sc, Expressions* /*fargs*/)
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
    // That's why we fix dedtypes by taking Inst's instantiation template args.

    auto c_ti = static_cast<cpp::TemplateInstance*>(ti);
    if (isForeignInstance(ti) && c_ti->primTiargs)
    {
        assert(dedtypes->dim == ti->tiargs->dim);
        for (unsigned i = 0; i < ti->tiargs->dim; i++)
            (*dedtypes)[i] = (*ti->tiargs)[i]; // tiargs were already deduced by Clang and fixed during correctTiargs
        return m;
    }

    auto& S = calypso.getSema();
    auto Temp = getPrimaryTemplate();

    TypeMapper tymap;
    ExprMapper expmap(tymap);
    tymap.addImplicitDecls = false;
    tymap.substsyms = collectSymbols(dedtypes);

    if (isa<clang::TypeAliasTemplateDecl>(Temp))
    {
        // Type alias instances do not remember their template args for some reason, which forces us to redo deduction.
        // This may be a temporary limitation of Clang.

        clang::TemplateArgumentListInfo ExplicitArgs;
        fillTemplateArgumentListInfo(ti->loc, sc, ExplicitArgs, ti->tiargs, Temp, tymap, expmap);

        auto fillDedtypes = [&] (llvm::ArrayRef<clang::TemplateArgument> Deduced)
        {
            assert(dedtypes->dim == Deduced.size());
            auto ParamList = Temp->getTemplateParameters();

            for (unsigned i = 0; i < dedtypes->dim; i++)
            {
                const clang::NamedDecl *Param = (i < ParamList->size()) ? ParamList->getParam(i) : nullptr;
                auto atype = TypeMapper::FromType(tymap, loc).fromTemplateArgument(&Deduced[i], Param);
                assert(atype);
                (*dedtypes)[i] = atype;
            }
        };

        llvm::SmallVector<clang::TemplateArgument, 4> Converted;

        if (S.CheckTemplateArgumentList(Temp, Temp->getLocation(), ExplicitArgs, false, Converted))
            assert(false && "Sema::CheckTemplateArgumentList failed on matching template");

        fillDedtypes(Converted);
    }
    else
    {
        auto Inst = getClangTemplateInst(sc, ti);
        auto InstArgs = (isForeignInstance(ti) ? getTemplateInstantiationArgs(Inst) : getTemplateArgs(Inst))->asArray();
        auto tdtypes = TypeMapper::FromType(tymap, loc).fromTemplateArguments(InstArgs.begin(), InstArgs.end(),
                        Temp->getTemplateParameters());

        SpecValue spec(tymap);
        getIdentifier(Inst, &spec, true);
        if (spec)
            tdtypes->shift(spec.toTemplateArg(loc));

        assert(tdtypes->dim == dedtypes->dim);
        for (unsigned i = 0; i < tdtypes->dim; i++)
            (*dedtypes)[i] = (*tdtypes)[i];
    }

    // Run semantic() on dedtypes using the scope for template parameters
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

    ::TemplateInstance::semanticTiargs(ti->loc, paramscope, dedtypes, 0);
    return m;
}

bool TemplateDeclaration::isForeignInstance(::TemplateInstance *ti)
{
    return isCPP(ti) && static_cast<TemplateInstance*>(ti)->isForeignInst;
}

clang::RedeclarableTemplateDecl *TemplateDeclaration::getPrimaryTemplate()
{
    if (auto RTD = dyn_cast<clang::RedeclarableTemplateDecl>(TempOrSpec))
        return const_cast<clang::RedeclarableTemplateDecl*>(RTD);

    if (auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(TempOrSpec))
        return CTSD->getSpecializedTemplate();

    if (auto FD = dyn_cast<clang::FunctionDecl>(TempOrSpec))
        return FD->getPrimaryTemplate();

    llvm_unreachable("Unhandled primary template");
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

    auto Temp = getPrimaryTemplate();

    cpp::TemplateInstance* ti;
    if (isCPP(tithis))
        ti = static_cast<cpp::TemplateInstance *>(
            tithis->syntaxCopy(nullptr));
    else
    {
        ti = new cpp::TemplateInstance(tithis->loc, tithis->name);
        tithis->syntaxCopy(ti);
    }

    if (ti->Inst)
    {
        // We query the evaluated arguments by Sema and possibly fix the tiargs semantic'd by DMD since there might be slight differences.
        // See for example Qt's is_unsigned<Qt::KeyboardModifiers>, the value will be different because of T(0) < T(-1) which doesn't lead to the expected result in DMD.
        // The issue wasn't apparent before because DMD and C++'s expression evaluation match in more than 99% of cases.
        // And this could become redundant if clang::Expr gets preserved and sent back to Clang for evaluation in the future.

        TypeMapper tymap;
        ExprMapper expmap(tymap);
        tymap.addImplicitDecls = false;
        tymap.cppPrefix = false;

        auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(ti->Inst);
        auto FuncSpec = dyn_cast<clang::FunctionDecl>(ti->Inst);

        auto& tiargs = *ti->tiargs;
        auto Args = ClassSpec ? ClassSpec->getTemplateArgs().asArray()
                : FuncSpec->getTemplateSpecializationArgs()->asArray();

        auto Arg = Args.begin();
        auto Param = Temp->getTemplateParameters()->begin();

        SpecValue spec(tymap);
        getIdentifierOrNull(Temp, &spec);

        for (int i = spec ? 1 : 0; i < ti->explicitargs; i++, Arg++, Param++)
        {
            if (!isExpression(tiargs[i]))
                continue;

            auto e = isExpression(
                    TypeMapper::FromType(tymap, loc).fromTemplateArgument(Arg, *Param));
            assert(e);
            tiargs[i] = e->semantic(globalScope(sc->instantiatingModule()));
        }
    }

    ti->semantictiargsdone = false;
    if (!ti->semanticTiargs(sc))
        assert(false && "foreignInstance semanticTiargs failed");

    if (!ti->Inst)
        ti->Inst = getClangTemplateInst(sc, ti);

    if (!ti->Inst)
    {
        assert(ti->errors);
        return ti;
    }

    makeForeignInstance(ti);

    ti->semanticRun = PASSinit; // WARNING: may disrupt something?
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

clang::NamedDecl* TemplateDeclaration::getClangTemplateInst(Scope* sc, ::TemplateInstance* ti)
{
    if (isCPP(ti))
        if (auto existingInst = static_cast<TemplateInstance*>(ti)->Inst)
            return existingInst;

    auto& S = calypso.getSema();
    auto& Diags = calypso.getDiagnostics();

    TypeMapper tymap;
    ExprMapper expmap(tymap);
    tymap.addImplicitDecls = false;
    tymap.cppPrefix = false;

    auto Temp = getPrimaryTemplate();

    clang::TemplateArgumentListInfo Args;
    fillTemplateArgumentListInfo(loc, sc, Args, ti->tiargs, Temp, tymap, expmap);

    clang::TemplateName Name(Temp);

    if (isa<clang::ClassTemplateDecl>(Temp) ||
            isa<clang::TypeAliasTemplateDecl>(Temp))
    {
        auto Ty = S.CheckTemplateIdType(Name, Temp->getLocation(), Args); // NOTE: this also substitutes the argument types
                                            // to TemplateTypeParmType, which is needed for partial specializations to work.

        auto RT = Ty->castAs<clang::RecordType>();
        auto CTSD = cast<clang::ClassTemplateSpecializationDecl>(RT->getDecl());
        return CTSD;
    }
    else if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(Temp))
    {
        assert(FuncTemp->getTemplatedDecl()->isDefined() ||
                FuncTemp->getInstantiatedFromMemberTemplate());

        auto FuncInst = instantiateFunctionDeclaration(Args, FuncTemp);

        if (!FuncInst)
        {
            Diags.Reset();
            ti->errors = true; // probably an attempt from functionResolve()
            return nullptr;
        }

        // Then the definition
        S.InstantiateFunctionDefinition(Temp->getLocation(), FuncInst, true);

        return FuncInst;
    }
    else
        assert(false);

    return nullptr;
}

::TemplateDeclaration* TemplateDeclaration::getCorrespondingTempDecl(clang::Decl* Inst)
{
    auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(Inst);
    auto FuncSpec = dyn_cast<clang::FunctionDecl>(Inst);

    if (isa<clang::TypeAliasTemplateDecl>(TempOrSpec)) // FIXME, any way to retrieve the template decl for alias templates?
        return this;

    const clang::Decl *Spec = ClassSpec;
    if (FuncSpec)
        Spec = FuncSpec;

    auto RealTemp = getSpecializedDeclOrExplicit(Spec)->getCanonicalDecl();

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
    auto Inst = ti->Inst;
    ti->tempdecl = getCorrespondingTempDecl(Inst);

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

bool TemplateInstance::completeInst()
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.getSema();
    auto& Diags = calypso.getDiagnostics();

    auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(Inst);

    if (CTSD && !CTSD->hasDefinition() &&
        CTSD->getSpecializedTemplate()->getTemplatedDecl()->hasDefinition()) // unused forward template specialization decls will exist but as empty aggregates
    {
        auto Ty = Context.getRecordType(CTSD);

        if (S.RequireCompleteType(CTSD->getLocation(), Ty, 0))
            Diags.Reset();
    }

    return true;
}

void TemplateInstance::correctTiargs()
{
    auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(Inst);

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

}