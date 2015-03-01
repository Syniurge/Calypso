// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "cpp/cppexpression.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cpptemplate.h"
#include "cpp/cpptypes.h"
#include "id.h"
#include "template.h"

#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

static Type *getAPIntDType(const llvm::APInt &i);

Objects *fromASTTemplateArgumentListInfo(
            const clang::ASTTemplateArgumentListInfo &Args,
            TypeMapper &tymap)
{
    auto tiargs = new Objects;

    for (unsigned i = 0; i < Args.NumTemplateArgs; i++)
    {
        auto Arg = &Args[i].getArgument();
        tiargs->push(TypeMapper::FromType(tymap).fromTemplateArgument(Arg));
    }

    return tiargs;
}

Expression* ExprMapper::fromUnaExp(const clang::UnaryOperator *E)
{
    auto loc = fromLoc(E->getLocStart());
    auto sub = fromExpression(E->getSubExpr());
    switch (E->getOpcode())
    {
        case clang::UO_Plus: return sub;
        case clang::UO_Minus: return new NegExp(loc, sub);
        case clang::UO_Not: return new NotExp(loc, sub);
        case clang::UO_LNot: return new NotExp(loc, sub);
        case clang::UO_Deref: return new PtrExp(loc, sub);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ unary operation exp");
}

Expression* ExprMapper::fromBinExp(const clang::BinaryOperator* E)
{
    auto loc = fromLoc(E->getLocStart());
    auto lhs = fromExpression(E->getLHS());
    auto rhs = fromExpression(E->getRHS());

    switch (E->getOpcode())
    {
        case clang::BO_Add: return new AddExp(loc, lhs, rhs);
        case clang::BO_Sub: return new MinExp(loc, lhs, rhs);
        case clang::BO_Mul: return new MulExp(loc, lhs, rhs);
        case clang::BO_Div: return new DivExp(loc, lhs, rhs);
        case clang::BO_Rem: return new ModExp(loc, lhs, rhs);

        case clang::BO_And: return new AndExp(loc, lhs, rhs);
        case clang::BO_Or: return new OrExp(loc, lhs, rhs);
        case clang::BO_Xor: return new XorExp(loc, lhs, rhs);

        case clang::BO_LAnd: return new AndAndExp(loc, lhs, rhs);
        case clang::BO_LOr: return new OrOrExp(loc, lhs, rhs);

        case clang::BO_Shl: return new ShlExp(loc, lhs, rhs);
        case clang::BO_Shr: return new ShrExp(loc, lhs, rhs);

        case clang::BO_LT: return new CmpExp(TOKlt, loc, lhs, rhs);
        case clang::BO_GT: return new CmpExp(TOKgt, loc, lhs, rhs);
        case clang::BO_LE: return new CmpExp(TOKle, loc, lhs, rhs);
        case clang::BO_GE: return new CmpExp(TOKge, loc, lhs, rhs);

        case clang::BO_EQ: return new EqualExp(TOKequal, loc, lhs, rhs);
        case clang::BO_NE: return new EqualExp(TOKnotequal, loc, lhs, rhs);

        case clang::BO_Assign: return new AssignExp(loc, lhs, rhs);
        case clang::BO_MulAssign: return new MulAssignExp(loc, lhs, rhs);
        case clang::BO_AddAssign: return new AddAssignExp(loc, lhs, rhs);
        case clang::BO_RemAssign: return new MinAssignExp(loc, lhs, rhs);
        case clang::BO_DivAssign: return new DivAssignExp(loc, lhs, rhs);

        case clang::BO_Comma: return new CommaExp(loc, lhs, rhs);
        case clang::BO_PtrMemD: return new DelegateFuncptrExp(loc, rhs);
        case clang::BO_PtrMemI: return new DelegateFuncptrExp(loc, rhs);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ binary operation exp");
}

Expression* ExprMapper::fromExpression(const clang::Expr* E, Type *destType,
                                            bool interpret)  // TODO implement interpret properly
{
    auto loc = fromLoc(E->getLocStart());
    Expression *e = nullptr;
    Type *t = nullptr;

    if (auto PE = dyn_cast<clang::ParenExpr>(E))
        return fromExpression(PE->getSubExpr());
    else if (auto ICE = dyn_cast<clang::CastExpr>(E))
        return fromExpression(ICE->getSubExpr(),
                            tymap.fromType(ICE->getType()));
    else if (auto CDA = dyn_cast<clang::CXXDefaultArgExpr>(E))
        return fromExpression(CDA->getExpr());

    if (auto UO = dyn_cast<clang::UnaryOperator>(E))
        return fromUnaExp(UO);
    else if (auto BO = dyn_cast<clang::BinaryOperator>(E))
        return fromBinExp(BO);
    else if (auto CO = dyn_cast<clang::ConditionalOperator>(E))
    {
        auto econd = fromExpression(CO->getCond());
        auto e1 = fromExpression(CO->getTrueExpr());
        auto e2 = fromExpression(CO->getFalseExpr());
        
        return new CondExp(loc, econd, e1, e2);
    }

    if (auto IL = dyn_cast<clang::IntegerLiteral>(E))
    {
        auto Val = IL->getValue();

        t = (!destType || !destType->isintegral()) ? getAPIntDType(Val) : destType;

        e = new IntegerExp(loc,
                Val.isNegative() ? Val.getSExtValue() : Val.getZExtValue(), t);
    }
    else if (auto CL = dyn_cast<clang::CharacterLiteral>(E))
    {
        auto Char = CL->getValue();

        switch (CL->getKind())
        {
            case clang::CharacterLiteral::Ascii:
                return new IntegerExp(loc, (d_uns8)Char, Type::tchar);
            case clang::CharacterLiteral::Wide:
            case clang::CharacterLiteral::UTF16:
                return new IntegerExp(loc, (d_uns16)Char, Type::twchar);
            case clang::CharacterLiteral::UTF32:
                return new IntegerExp(loc, (d_uns32)Char, Type::tdchar);
        }
    }
    else if (auto BL = dyn_cast<clang::CXXBoolLiteralExpr>(E))
    {
        return new IntegerExp(loc, BL->getValue() ? 1 : 0, Type::tbool);
    }
    else if (auto FL = dyn_cast<clang::FloatingLiteral>(E))
    {
        auto APFVal = FL->getValue();

        real_t val;
        Type *ft = Type::tfloat32;

        if (APFVal.isZero())
            val = 0.0;
        else if (&APFVal.getSemantics() == &llvm::APFloat::IEEEsingle)
            val = APFVal.convertToFloat();
        else if (&APFVal.getSemantics() == &llvm::APFloat::IEEEdouble)
        {
            val = APFVal.convertToDouble();
            ft = Type::tfloat64;
        }
        else
        {
            ::warning(loc, "Floating point semantics for non-zero APFloat handled by converting to string and strtold");

            llvm::SmallString<16> Str;
            APFVal.toString(Str, 0, llvm::APFloat::semanticsPrecision(APFVal.getSemantics()));
            val = strtold(Str.c_str(), nullptr);
            ft = Type::tfloat80;
        }

        return new RealExp(loc, val, ft);
    }
    else if (auto SL = dyn_cast<clang::StringLiteral>(E))
    {
        utf8_t postfix = 0;
        if (SL->getCharByteWidth() == 2)
            postfix = 'w';
        else if (SL->getCharByteWidth() == 4)
            postfix = 'd';
        else assert(SL->getCharByteWidth() == 0);

        return new StringExp(loc, const_cast<char*>(SL->getString().data()),
                             SL->getLength(), postfix);
    }
    else if (isa<clang::CXXNullPtrLiteralExpr>(E) || isa<clang::GNUNullExpr>(E))
    {
        return new NullExp(loc);
    }

    if (auto TT = dyn_cast<clang::TypeTraitExpr>(E))
    {
        if (!TT->isValueDependent())
            return new IntegerExp(loc, TT->getValue() ? 1 : 0, Type::tbool);
        else
            return new NullExp(loc);  // TODO replace by D traits
    }
    else if (auto SOP = dyn_cast<clang::SizeOfPackExpr>(E))
    {
        if (!SOP->isValueDependent())
            return new IntegerExp(loc, SOP->getPackLength(), Type::tuns64);
        else
            return new NullExp(loc);  // TODO replace by D traits
    }
    else if (auto NE = dyn_cast<clang::CXXNoexceptExpr>(E))
    {
        if (!NE->isValueDependent())
            return new IntegerExp(loc, NE->getValue() ? 1 : 0, Type::tbool);
        else
            return new NullExp(loc);
    }
    else if (auto UEOTT = dyn_cast<clang::UnaryExprOrTypeTraitExpr>(E))
    {
        auto t = tymap.fromType(UEOTT->getTypeOfArgument());
        auto e1 = new TypeExp(loc, t);

        switch (UEOTT->getKind())
        {
            case clang::UETT_SizeOf:
                return new DotIdExp(loc, e1, Id::__sizeof);
            case clang::UETT_AlignOf:
                return new DotIdExp(loc, e1, Id::__xalignof);
            default:
                assert(false && "Unsupported");
        }
    }

    if (auto DR = dyn_cast<clang::DeclRefExpr>(E))
    {
        return fromExpressionDeclRef(loc,
                            const_cast<clang::ValueDecl*>(DR->getDecl()));
    }

    if (auto PE = dyn_cast<clang::PackExpansionExpr>(E))
    {
        return fromExpression(PE->getPattern());
    }
    else if (auto SNTTP = dyn_cast<clang::SubstNonTypeTemplateParmExpr>(E))
    {
        if (!interpret || SNTTP->isValueDependent())
            return fromExpressionNonTypeTemplateParm(loc,
                                SNTTP->getParameter());
        else
            return fromExpression(SNTTP->getReplacement());
    }
    else if (auto DSDR = dyn_cast<clang::DependentScopeDeclRefExpr>(E))
    {
        Expression *e1 = nullptr;
        Identifier *ident;
        ::TemplateInstance *tempinst = nullptr;

        if (auto NNS = DSDR->getQualifier())
        {
            auto tqual = TypeMapper::FromType(tymap).fromNestedNameSpecifier(NNS);
            e1 = new TypeExp(loc, tqual);
        }

        if (DSDR->getDeclName().isIdentifier())
            ident = fromIdentifier(DSDR->getDeclName().getAsIdentifierInfo());
        else
            assert(false && "Unhandled DeclarationName kind");

        if (DSDR->hasExplicitTemplateArgs())
        {
            auto tiargs = fromASTTemplateArgumentListInfo(
                              DSDR->getExplicitTemplateArgs(), tymap);

            tempinst = new ::TemplateInstance(loc, ident);
            tempinst->tiargs = tiargs;
        }

        if (e1)
        {
            if (tempinst)
                return new DotTemplateInstanceExp(loc, e1, tempinst);
            else
                return new DotIdExp(loc, e1, ident);
        }
        else
        {
            if (tempinst)
                return new TypeExp(loc, new TypeInstance(loc, tempinst));
            else
                return new IdentifierExp(loc, ident);
        }
    }
    else if (auto C = dyn_cast<clang::CallExpr>(E))
    {
        auto e = fromExpression(C->getCallee());

        auto args = new Expressions;
        for (auto Arg: C->arguments())
            args->push(fromExpression(Arg));

        return new CallExp(loc, e, args);
    }
    else if (auto UL = dyn_cast<clang::UnresolvedLookupExpr>(E))
    {
        if (!UL->getName().isIdentifier())
        {
            ::warning(loc, "Unsupported DeclarationName for UnresolvedLookupExpr");
            return new NullExp(loc);
        }

        // FIXME NNS
        auto id = fromIdentifier(UL->getName().getAsIdentifierInfo());

        if (UL->getNumTemplateArgs())
        {
            auto& ExplicitTempArgs = UL->getExplicitTemplateArgs();

            auto tempinst = new ::TemplateInstance(loc, id);
            tempinst->tiargs = fromASTTemplateArgumentListInfo(ExplicitTempArgs, tymap);
            return new ScopeExp(loc, tempinst);
        }
        else
            return new IdentifierExp(loc, id);
    }
    else if (auto SVI = dyn_cast<clang::CXXScalarValueInitExpr>(E))
    {
        t = tymap.fromType(E->getType());
        return new DotIdExp(loc, new TypeExp(loc, t), Id::init);
    }
    else if (auto MT = dyn_cast<clang::MaterializeTemporaryExpr>(E))
    {
        auto Ty = MT->GetTemporaryExpr()->getType();
        t = tymap.fromType(Ty);

        // NOTE: if the context is dependent, wrongly deferencing a "new SomeClass" is of no importance except for reflection

        e = new NewExp(loc, nullptr, nullptr, t, nullptr);
        if (!isNonPODRecord(Ty))
            e = new PtrExp(loc, e);
    }

    if (isa<clang::InitListExpr>(E)) //TODO
        return new NullExp(loc);

    if (isa<clang::CXXConstructExpr>(E) ||
        isa<clang::CXXUnresolvedConstructExpr>(E) ||
        isa<clang::CXXNewExpr>(E)) // TODO implement evaluation
        return new NullExp(loc) /* nullptr */;

    if (e)
    {
        if (!t || !destType ||
                t->implicitConvTo(destType) > MATCHconvert)
            return e;

        return new CastExp(loc, e, destType);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ expression");
}

Type *getAPIntDType(const llvm::APInt &i)
{
    bool needs64bits = i.getBitWidth() > 32;

    if (i.isNegative())
        return needs64bits ? Type::tint64 : Type::tint32;
    else
        return needs64bits ? Type::tuns64 : Type::tuns32;
}

Expression* ExprMapper::fromAPInt(const APInt& Val)
{
    return new IntegerExp(Loc(),
            Val.isNegative() ? Val.getSExtValue() : Val.getZExtValue(),
            getAPIntDType(Val));
}

Expression* ExprMapper::fromExpressionDeclRef(Loc loc, clang::NamedDecl *D)
{
    if (auto NTTP = dyn_cast<clang::NonTypeTemplateParmDecl>(D))
        return fromExpressionNonTypeTemplateParm(loc, NTTP);

    auto tqual = static_cast<TypeQualified*>(TypeMapper::FromType(tymap).typeQualifiedFor(D));
    assert(tqual && "DeclRefExpr decl without a DeclarationName");

    // TODO: Build a proper expression from the type (mostly for reflection and to mimic parse.c, since TypeExp seems to work too)
    return new TypeExp(loc, tqual);
}

Expression* ExprMapper::fromExpressionNonTypeTemplateParm(Loc loc, const clang::NonTypeTemplateParmDecl* D)
{
    auto ident = DeclMapper::getIdentifierForTemplateNonTypeParm(D);
    return new IdentifierExp(loc, ident);
}

clang::Expr* ExprMapper::toExpression(Expression* e)
{
    auto& Context = calypso.pch.AST->getASTContext();
    clang::SourceLocation Loc;

    switch (e->op)
    {
        case TOKint64:
        {
            auto exp = static_cast<IntegerExp*>(e);
            auto value = exp->getInteger();

            if (e->type->ty == Tbool)
                return new (Context) clang::CXXBoolLiteralExpr(value != 0,
                                                               Context.BoolTy, Loc);
            else
            {
                unsigned IntSize = Context.getTargetInfo().getIntWidth();
                return clang::IntegerLiteral::Create(Context,
                                                llvm::APInt(IntSize, value),
                                                Context.IntTy, Loc);
            }
        }
        case TOKfloat64:
        {
            auto exp = static_cast<RealExp *>(e);
            llvm::APFloat Val((double) exp->value);
            return clang::FloatingLiteral::Create(Context,
                                            Val, true,
                                            Context.FloatTy, Loc);
        }
        default:
            llvm::llvm_unreachable_internal("Unhandled D -> Clang expression conversion");
    }
}

}
