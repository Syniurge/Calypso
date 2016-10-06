// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cppexpression.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cpptemplate.h"
#include "cpp/cpptypes.h"
#include "aggregate.h"
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

static Type *getAPIntDType(const llvm::APSInt &i);

Expression *dotIdentOrInst(Loc loc, Expression *e1, RootObject *o)
{
    if (o->dyncast() == DYNCAST_IDENTIFIER)
    {
        auto ident = static_cast<Identifier*>(o);
        if (!e1)
            return new IdentifierExp(loc, ident);
        else
            return new DotIdExp(loc, e1, ident);
    }
    else
    {
        assert(o->dyncast() == DYNCAST_DSYMBOL && static_cast<Dsymbol*>(o)->isTemplateInstance());
        auto tempinst = static_cast<::TemplateInstance*>(o);
        if (!e1)
            return new ScopeExp(loc, tempinst);
        else
            return new DotTemplateInstanceExp(loc, e1, tempinst);
    }
}

RootObject *typeQualifiedRoot(TypeQualified *tqual)
{
    if (tqual->ty == Tident)
        return static_cast<TypeIdentifier*>(tqual)->ident;
    else if (tqual->ty == Tinstance)
        return static_cast<TypeInstance*>(tqual)->tempinst;

    llvm_unreachable("FIXME TypeOf");
}

Objects *fromASTTemplateArgumentListInfo(Loc loc,
            const clang::TemplateArgumentLoc *Args,
            unsigned NumTemplateArgs,
            TypeMapper &tymap)
{
    auto tiargs = new Objects;

    for (unsigned i = 0; i < NumTemplateArgs; i++)
    {
        auto Arg = &Args[i].getArgument();
        tiargs->append(TypeMapper::FromType(tymap, loc).fromTemplateArgument(Arg));
    }

    return tiargs;
}

Expression *ExprMapper::fixIntegerExp(IntegerExp *e, clang::QualType T)
{
    if (auto ET = T->getAs<clang::EnumType>())
    {
        // In Clang AST enum values in template arguments are resolved to integer literals
        // If the parameter has an enum type, we need to revert integer literals to DeclRefs pointing to enum constants
        // or else DMD won't find the template decl since from its point of view uint != Enum
        // Also useful for function default zero arguments

        for (auto ECD: ET->getDecl()->enumerators())
        {
            auto Val = ECD->getInitVal().getZExtValue();

            if (Val == e->getInteger())
                return fromExpressionDeclRef(e->loc, ECD);
        }

        return new CastExp(e->loc, e, tymap.fromType(T, e->loc));
        //llvm_unreachable("Couldn't find the corresponding enum constant");
    }

    auto BT = T->getAs<clang::BuiltinType>();
    if (BT && BT->getKind() == clang::BuiltinType::Char_S)
    {
        // C++ char may be signed, unlike D's, yet mapping char to uns8 instead of char
        // would be very destructive since character and string literals wouldn't work
        // In special cases such as numeric_traits::min we convert negative values to unsigned during the mapping.
        // The value won't be correct but the implicit conversion won't fail.
        if (!e->type->isunsigned() && static_cast<sinteger_t>(e->getInteger()) < 0)
            e = new IntegerExp(e->loc, static_cast<unsigned char>(e->getInteger()), Type::tchar);
    }

    return e;
}

inline bool isMemberFuncPtrCall(const clang::Expr* E)
{
    if (!isa<clang::BinaryOperator>(E))
        return false;

    auto Op = cast<clang::BinaryOperator>(E)->getOpcode();
    return Op == clang::BO_PtrMemD || Op == clang::BO_PtrMemD;
}

Expression *ExprMapper::fromUnaExp(clang::SourceLocation Loc,
                                   const clang::UnaryOperator::Opcode Op,
                                   const clang::Expr *SubExpr)
{
    auto loc = fromLoc(Loc);
    auto sub = fromExpression(SubExpr);

    if (!sub)
        return nullptr;

    switch (Op)
    {
        case clang::UO_Plus: return sub;
        case clang::UO_Minus: return new NegExp(loc, sub);
        case clang::UO_Not: return new NotExp(loc, sub);
        case clang::UO_LNot: return new NotExp(loc, sub);
        case clang::UO_AddrOf: return new AddrExp(loc, sub);
        case clang::UO_Deref: return new PtrExp(loc, sub);

        case clang::UO_PreInc: return new PreExp(TOKplusplus, loc, sub);
        case clang::UO_PostInc: return new PreExp(TOKminusminus, loc, sub);
        case clang::UO_PreDec: return new PostExp(TOKplusplus, loc, sub);
        case clang::UO_PostDec: return new PostExp(TOKminusminus, loc, sub);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ unary operation exp");
}

Expression* ExprMapper::fromUnaExp(const clang::UnaryOperator *E)
{
    return fromUnaExp(E->getLocStart(), E->getOpcode(), E->getSubExpr());
}

Expression *ExprMapper::fromBinExp(clang::SourceLocation Loc,
                                const clang::BinaryOperator::Opcode Op,
                                const clang::Expr *LHS, const clang::Expr *RHS)
{
    auto loc = fromLoc(Loc);
    auto lhs = fromExpression(LHS);
    auto rhs = fromExpression(RHS);

    if (!lhs || !rhs)
        return nullptr;

    switch (Op)
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
        case clang::BO_SubAssign: return new MinAssignExp(loc, lhs, rhs);
        case clang::BO_DivAssign: return new DivAssignExp(loc, lhs, rhs);
        case clang::BO_RemAssign: return new ModAssignExp(loc, lhs, rhs);

        case clang::BO_AndAssign: return new AndAssignExp(loc, lhs, rhs);
        case clang::BO_OrAssign: return new OrAssignExp(loc, lhs, rhs);
        case clang::BO_XorAssign: return new XorAssignExp(loc, lhs, rhs);
        case clang::BO_ShlAssign: return new ShlAssignExp(loc, lhs, rhs);
        case clang::BO_ShrAssign: return new ShrAssignExp(loc, lhs, rhs);

        case clang::BO_Comma: return new CommaExp(loc, lhs, rhs);

        // Member function pointers calls (.* and ->*) using UFCS
        // TODO Itanium virtual function pointers
        case clang::BO_PtrMemD: return new CallExp(loc, new PtrExp(loc, rhs), lhs);
        case clang::BO_PtrMemI: return new CallExp(loc, new PtrExp(loc, rhs), lhs);
    }

    llvm::llvm_unreachable_internal("Unhandled C++ binary operation exp");
}

Expression* ExprMapper::fromBinExp(const clang::BinaryOperator* E)
{
    return fromBinExp(E->getLocStart(), E->getOpcode(),
                    E->getLHS(), E->getRHS());
}

static const clang::Expr* skipIgnoredCast(const clang::CastExpr *E)
{
    auto Kind = E->getCastKind();
    auto SubExpr = E->getSubExpr();

    bool skipCast = false;
    if (isa<clang::ImplicitCastExpr>(E))
    {
        skipCast = true;

        if (Kind == clang::CK_NullToMemberPointer)
            skipCast = false; // tptrdiff_t is a signed int type, nullptr needs to be translated to 0

        // One exception being if the subexpr is an enum constant, in which case handling the cast to specify the signedness of the expression
        // will prevent some errors during function resolution which overloads for both signed and unsigned arguments.
        if (Kind == clang::CK_IntegralCast && SubExpr->getType()->isEnumeralType())
            skipCast = false;
    }

    auto CastDestTy = E->getType();

    switch(Kind)
    {
        case clang::CK_NullToPointer:
            skipCast = false;
            break;
        case clang::CK_NoOp:
        case clang::CK_ConstructorConversion:
        case clang::CK_LValueToRValue:
            skipCast = true;
            break;
        case clang::CK_Dependent:
            if (SubExpr->getType().getCanonicalType() == CastDestTy.getCanonicalType())
                skipCast = true;
            break;
        default:
            break;
    }

    return skipCast ? SubExpr : E;
}

static const clang::Expr* skipIgnored(const clang::Expr *E)
{
    const clang::Expr* SubExpr = E;

    if (auto CastExpr = dyn_cast<clang::CastExpr>(E))
        SubExpr = skipIgnoredCast(CastExpr);
    else if (auto ConstructExpr = dyn_cast<clang::CXXConstructExpr>(E))
    {
        if (ConstructExpr->getNumArgs())
        {
            auto Ctor = ConstructExpr->getConstructor();
            auto Arg0 = ConstructExpr->getArg(0);

            if (ConstructExpr->isElidable() ||
                    (Ctor->isCopyConstructor() && Ctor->isTrivial() && isa<clang::MaterializeTemporaryExpr>(Arg0)))
                SubExpr = Arg0;
        }
    }

    return (SubExpr != E) ? skipIgnored(SubExpr) : E;
}

Expression *ExprMapper::fromCastExpr(Loc loc, const clang::CastExpr *E)
{
    auto Kind = E->getCastKind();

    if (Kind == clang::CK_NullToPointer)
        return new NullExp(loc);
    else if (Kind == clang::CK_NullToMemberPointer)
        return new IntegerExp(loc, 0, Type::tptrdiff_t);

    auto SubExpr = E->getSubExpr();
    auto CastDestTy = E->getType();
    auto e = fromExpression(SubExpr);

    if (skipIgnoredCast(E) != E)
        return e;

    assert(SubExpr->getType().getCanonicalType()
                    != CastDestTy.getCanonicalType()); // we should be ignoring all casts that do not alter the type

    return new CastExp(loc, e, tymap.fromType(CastDestTy, loc));
}

Expression* ExprMapper::fromExpression(const clang::Expr *E, bool interpret)  // TODO implement interpret properly
{
    auto loc = fromLoc(E->getLocStart());
    E = skipIgnored(E);

    Expression *e = nullptr;
    Type *t = nullptr;
    clang::QualType Ty;

    if (auto Cast = dyn_cast<clang::CastExpr>(E))
        return fromCastExpr(loc, Cast);

    else if (auto PE = dyn_cast<clang::ParenExpr>(E))
        e = fromExpression(PE->getSubExpr());
    else if (auto CDA = dyn_cast<clang::CXXDefaultArgExpr>(E))
        e = fromExpression(CDA->getExpr());
    else if (auto EWC = dyn_cast<clang::ExprWithCleanups>(E))
        e = fromExpression(EWC->getSubExpr());

    else if (auto UO = dyn_cast<clang::UnaryOperator>(E))
        e = fromUnaExp(UO);
    else if (auto BO = dyn_cast<clang::BinaryOperator>(E))
        e = fromBinExp(BO);
    else if (auto CO = dyn_cast<clang::ConditionalOperator>(E))
    {
        auto econd = fromExpression(CO->getCond());
        auto e1 = fromExpression(CO->getTrueExpr());
        auto e2 = fromExpression(CO->getFalseExpr());

        if (!econd || !e1 || !e2)
            return nullptr;
        
        e = new CondExp(loc, econd, e1, e2);
    }

    else if (auto IL = dyn_cast<clang::IntegerLiteral>(E))
    {
        auto Val = IL->getValue();
        Ty = E->getType();
        t = tymap.fromType(Ty, loc);

        e = new IntegerExp(loc, Ty->hasSignedIntegerRepresentation() ?
                        Val.getSExtValue() : Val.getZExtValue(), t);

        // D won't be as lenient as C++ is towards signed constants overflowing into negative values,
        // so even if Type::implicitConvTo matches we should still check the evaluated expression
        // (see _ISwgraph in wctype.h for an example of this special case)
//         if (!E->isInstantiationDependent() && !destType->isunsigned())
//         {
//             llvm::APSInt V(Val, Ty->hasUnsignedIntegerRepresentation());
//             if (E->EvaluateAsInt(V, Context))
//                 if (V.isUnsigned() && V.getActiveBits() == Context.getIntWidth(Ty))
//                     e = new CastExp(loc, e, destType);
//         }
    }
    else if (auto CL = dyn_cast<clang::CharacterLiteral>(E))
    {
        auto Char = CL->getValue();

        switch (CL->getKind())
        {
            case clang::CharacterLiteral::Ascii:
            case clang::CharacterLiteral::UTF8:
                t = Type::tchar;
                e = new IntegerExp(loc, (d_uns8)Char, t);
                break;
            case clang::CharacterLiteral::Wide:
            case clang::CharacterLiteral::UTF16:
                t = Type::twchar;
                e = new IntegerExp(loc, (d_uns16)Char, t);
                break;
            case clang::CharacterLiteral::UTF32:
                t = Type::tdchar;
                e = new IntegerExp(loc, (d_uns32)Char, t);
                break;
        }
    }
    else if (auto BL = dyn_cast<clang::CXXBoolLiteralExpr>(E))
    {
        t = Type::tbool;
        e = new IntegerExp(loc, BL->getValue() ? 1 : 0, t);
    }
    else if (auto FL = dyn_cast<clang::FloatingLiteral>(E))
    {
        auto APFVal = FL->getValue();
        e = fromAPFloat(loc, APFVal, &t);
    }
    else if (auto SL = dyn_cast<clang::StringLiteral>(E))
    {
        utf8_t postfix = 0;
        if (SL->getCharByteWidth() == 2)
        {
            postfix = 'w';
            t = new TypeDArray(Type::tdchar->immutableOf());
                // WARNING: string literals with postfixes need to be explicitly cast to pointer types (DMD BUG?)
                // which is being done at the ImplicitCastExpr level.
                // See https://issues.dlang.org/show_bug.cgi?id=6032
        }
        else if (SL->getCharByteWidth() == 4)
        {
            postfix = 'd';
            t = new TypeDArray(Type::twchar->immutableOf());
        }
        else assert(SL->getCharByteWidth() == 1);

        e = new StringExp(loc, const_cast<char*>(SL->getBytes().data()),
                             SL->getLength(), postfix);
    }
    else if (isa<clang::CXXNullPtrLiteralExpr>(E) || isa<clang::GNUNullExpr>(E))
    {
        return new NullExp(loc);
    }

    else if (auto TT = dyn_cast<clang::TypeTraitExpr>(E))
    {
        if (!TT->isValueDependent())
            e = new IntegerExp(loc, TT->getValue() ? 1 : 0, Type::tbool);
        else
        {
            switch (TT->getTrait())
            {
                case clang::UTT_IsPOD:
                {
                    auto args = new Objects;
                    auto t = tymap.fromType(TT->getArg(0)->getType(), loc);
                    args->push(t);
                    e = new TraitsExp(loc, Id::isPOD, args);
                    break;
                }
                default:
                    return new NullExp(loc);  // TODO replace by D traits
            }
        }
    }
    else if (auto SOP = dyn_cast<clang::SizeOfPackExpr>(E))
    {
        if (!SOP->isValueDependent())
            e = new IntegerExp(loc, SOP->getPackLength(), Type::tuns64);
        else
            return new NullExp(loc);  // TODO replace by D traits
    }
    else if (auto NE = dyn_cast<clang::CXXNoexceptExpr>(E))
    {
        if (!NE->isValueDependent())
            e = new IntegerExp(loc, NE->getValue() ? 1 : 0, Type::tbool);
        else
            return new NullExp(loc);
    }
    else if (auto UEOTT = dyn_cast<clang::UnaryExprOrTypeTraitExpr>(E))
    {
        auto t = tymap.fromType(UEOTT->getTypeOfArgument(), loc);
        auto e1 = new TypeExp(loc, t);

        switch (UEOTT->getKind())
        {
            case clang::UETT_SizeOf:
                e = new DotIdExp(loc, e1, Id::__sizeof);
                break;
            case clang::UETT_AlignOf:
                e = new DotIdExp(loc, e1, Id::__xalignof);
                break;
            default:
                assert(false && "Unsupported");
        }
    }

    else if (auto DR = dyn_cast<clang::DeclRefExpr>(E))
    {
        Ty = DR->getType();
        t = tymap.fromType(Ty, loc);

        e = fromExpressionDeclRef(loc, const_cast<clang::ValueDecl*>(DR->getDecl()),
                        DR->getQualifier());
    }

    else if (auto PE = dyn_cast<clang::PackExpansionExpr>(E))
    {
        e = fromExpression(PE->getPattern());
    }
    else if (auto SNTTP = dyn_cast<clang::SubstNonTypeTemplateParmExpr>(E))
    {
        //if (SNTTP->isValueDependent())
        //    e = fromExpressionNonTypeTemplateParm(loc, SNTTP->getParameter());
        //else
        assert(SNTTP->getReplacement());
        e = fromExpression(SNTTP->getReplacement());
    }
    else if (auto MExpr = dyn_cast<clang::MemberExpr>(E))
    {
        return fromExpressionMemberExpr(loc, MExpr, MExpr->getMemberDecl()->getDeclName());
    }
    else if (auto CDSM = dyn_cast<clang::CXXDependentScopeMemberExpr>(E))
    {
        return fromExpressionMemberExpr(loc, CDSM, CDSM->getMember());
    }
    else if (auto DSDR = dyn_cast<clang::DependentScopeDeclRefExpr>(E))
    {
        Expression *e1 = nullptr;
        Identifier *ident;
        ::TemplateInstance *tempinst = nullptr;

        if (auto NNS = DSDR->getQualifier())
        {
            auto tqual = TypeMapper::FromType(tymap, loc).fromNestedNameSpecifier(NNS);
            e1 = new TypeExp(loc, tqual);
        }

        if (DSDR->getDeclName().isIdentifier())
            ident = fromIdentifier(DSDR->getDeclName().getAsIdentifierInfo());
        else
            assert(false && "Unhandled DeclarationName kind");

        if (DSDR->hasExplicitTemplateArgs())
        {
            auto tiargs = fromASTTemplateArgumentListInfo(loc,
                        DSDR->getTemplateArgs(), DSDR->getNumTemplateArgs(), tymap);

            tempinst = new ::TemplateInstance(loc, ident);
            tempinst->tiargs = tiargs;
        }

        if (e1)
        {
            if (tempinst)
                e = new DotTemplateInstanceExp(loc, e1, tempinst);
            else
                e = new DotIdExp(loc, e1, ident);
        }
        else
        {
            if (tempinst)
                e = new TypeExp(loc, new TypeInstance(loc, tempinst));
            else
                e = new IdentifierExp(loc, ident);
        }
    }
    else if (auto C = dyn_cast<clang::CallExpr>(E))
    {
        if (auto OC = dyn_cast<clang::CXXOperatorCallExpr>(E))
        {
            // Since calling opBinary!"+"(...) won't work if there multiple opBinary templates, prefer the operator expression when possible so that overloaded operator resolution kicks in.
            // TODO: note that the mapping won't always result in correct values as long as non member operators don't take part in D's overloaded operator resolution
            // Additionally in dependent contexts operator calls aren't resolved yet to UnaryOperator and BinaryOperator, which are easier on the eyes

            auto OO = OC->getOperator();
            if (C->getNumArgs() == 2 && OO >= clang::OO_Plus && OO <= clang::OO_Arrow &&
                    OO != clang::OO_PlusPlus && OO != clang::OO_MinusMinus)
            {
                auto Op = clang::BinaryOperator::getOverloadedOpcode(OO);
                auto LHS = C->getArg(0), RHS = C->getArg(1);
                e = fromBinExp(E->getLocStart(), Op, LHS, RHS);
            }
            else if (C->getNumArgs() == 1 && OO < clang::OO_Call)
            {
                auto Op = clang::UnaryOperator::getOverloadedOpcode(OO, false); // WARNING: how to determine whether prefix or postfix?
                auto Sub = C->getArg(0);
                e = fromUnaExp(E->getLocStart(), Op, Sub);
            }
        }

        if (!e)
        {
            auto callee = fromExpression(C->getCallee());
            if (!callee)
                return nullptr;

            Expressions* args;
            if (isMemberFuncPtrCall(C->getCallee()))
            {
                assert(callee->op == TOKcall);
                args = static_cast<CallExp*>(callee)->arguments;
            }
            else
                args = new Expressions;

            for (auto Arg: C->arguments())
                args->push(fromExpression(Arg));

            e = new CallExp(loc, callee, args);
        }
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

        if (UL->hasExplicitTemplateArgs())
        {
            auto tempinst = new ::TemplateInstance(loc, id);
            tempinst->tiargs = fromASTTemplateArgumentListInfo(loc,
                        UL->getTemplateArgs(), UL->getNumTemplateArgs(), tymap);
            e = new ScopeExp(loc, tempinst);
        }
        else
            e = new IdentifierExp(loc, id);
    }
    else if (isa<clang::CXXScalarValueInitExpr>(E))
    {
        if (E->getType()->getAs<clang::ReferenceType>())
            return new NullExp(loc);

        t = tymap.fromType(E->getType().withoutLocalFastQualifiers(), loc);
        e = new CallExp(loc, new TypeExp(loc, t));
    }
    else if (auto MT = dyn_cast<clang::MaterializeTemporaryExpr>(E))
    {
        auto Ty = E->getType();
        auto TempExpr = skipIgnored(MT->GetTemporaryExpr());  // ignore skipped casts, elidable constructs, etc. in order to not bias TempExpr->isLValue()
        e = fromExpression(TempExpr);

        if (!e)
            return nullptr;

//         if (Ty->getAs<clang::ReferenceType>() && e->op == TOKnull)
//             return nullptr; // FIXME once we may finally pass rvalues to ref arguments
//                     // for other types there are workarounds but for null class references
//                     // I couldn't find any way to turn them into lvalues.
//
//         if (!TempExpr->isLValue())
//         {
//             if (e->op == TOKcall &&
//                     static_cast<CallExp*>(e)->e1->op == TOKtype)
//             {
//                 auto call = static_cast<CallExp*>(e);
//                 e = new NewExp(loc, nullptr, nullptr,
//                                call->e1->type, call->arguments);
//             }
//             else
//             {
//                 t = tymap.fromType(Ty.withoutLocalFastQualifiers(), loc);
//
//                 auto args = new Expressions;
//                 args->push(e);
//
//                 e = new NewExp(loc, nullptr, nullptr, t, args);
//             }
//
//             e = new PtrExp(loc, e);
//         }
    }
    else if (auto CBT = dyn_cast<clang::CXXBindTemporaryExpr>(E))
    {
        e = fromExpression(CBT->getSubExpr());
    }
    else if (auto CCE = dyn_cast<clang::CXXConstructExpr>(E))
    {
        if (CCE->isElidable())
            e = fromExpression(CCE->getArg(0));
        else
        {
            t = tymap.fromType(E->getType().withoutLocalFastQualifiers(), loc);

            auto args = new Expressions;
            for (auto Arg: CCE->arguments())
                args->push(fromExpression(Arg));

            e = new CallExp(loc, new TypeExp(loc, t), args);
        }
    }
    else if (auto CNE = dyn_cast<clang::CXXNewExpr>(E))
    {
        auto Ty = CNE->getAllocatedType();
        t = tymap.fromType(Ty.withoutLocalFastQualifiers(), loc);

        Expressions *args = nullptr;
        auto Construct = CNE->getConstructExpr();
        if (Construct && !Construct->isElidable())
        {
            args = new Expressions;
            for (auto Arg: Construct->arguments())
                args->push(fromExpression(Arg));
        }

        e = new NewExp(loc, nullptr, nullptr, t, args);
    }

    else if (isa<clang::CXXThisExpr>(E))
        return new ThisExp(loc);

    else if (isa<clang::InitListExpr>(E)) // TODO
        return new NullExp(loc);

    else if (isa<clang::CXXUnresolvedConstructExpr>(E)
        || isa<clang::CXXPseudoDestructorExpr>(E))
        return new NullExp(loc) /* nullptr */;

    else
        llvm::llvm_unreachable_internal("Unhandled C++ expression");

    return e;
}

Type *getAPIntDType(const llvm::APSInt &i)
{
    bool needs64bits = i.getBitWidth() > 32;

    if (i.isSigned())
        return needs64bits ? Type::tint64 : Type::tint32;
    else
        return needs64bits ? Type::tuns64 : Type::tuns32;
}

Expression *ExprMapper::fromAPValue(Loc loc, const clang::APValue &Val,
                        clang::QualType Ty, Expression **result)
{
    using clang::APValue;

    switch (Val.getKind())
    {
        case APValue::Int:
            return fromAPInt(loc, Val.getInt(), Ty);
        case APValue::Float:
            return fromAPFloat(loc, Val.getFloat());
        case APValue::Struct:
        {
            if (!result)
                return nullptr; // FIXME DeclMapper::VisitValueDecl relies on fromAPValue being limited, fix it

            assert((*result)->op == TOKstructliteral);
            auto sle = static_cast<StructLiteralExp*>(*result);

            size_t elem_i = 0;
            std::function<void(const APValue &Val, AggregateDeclaration* ad)>
                fromFields = [&] (const APValue &Val, AggregateDeclaration* ad)
            {
                auto cd = ad->isClassDeclaration();

                unsigned numBases = 0;
                if (cd && cd->baseclasses)
                    numBases = cd->baseclasses->dim;

                if (auto base = toAggregateBase(ad))  // FIXME numBases > 1
                    fromFields(Val.getStructBase(0), base);

                for (size_t i = 0; i < ad->fields.dim; i++, elem_i++) {
                    assert(elem_i < sle->elements->dim);
                    (*sle->elements)[elem_i] = fromAPValue(loc, Val.getStructField(i),
                                    clang::QualType(), &(sle->elements->data[i]));
                }

                return true;
            };

            fromFields(Val, sle->sd);
            return sle;
        }
        default:
            return nullptr;
    }
}

Expression* ExprMapper::fromAPInt(Loc loc, const llvm::APSInt &Val,
                                  clang::QualType Ty)
{
    auto e = new IntegerExp(loc,
            Val.isSigned() ? Val.getSExtValue() : Val.getZExtValue(),
            getAPIntDType(Val));

    if (!Ty.isNull())
        return fixIntegerExp(e, Ty);
    else
        return e;
}

Expression* ExprMapper::fromAPFloat(Loc loc, const APFloat& Val, Type **pt)
{
    real_t val;
    Type *t = Type::tfloat32;

    if (Val.isZero())
        val = 0.0;
    else if (&Val.getSemantics() == &llvm::APFloat::IEEEsingle)
        val = Val.convertToFloat();
    else if (&Val.getSemantics() == &llvm::APFloat::IEEEdouble)
    {
        val = Val.convertToDouble();
        t = Type::tfloat64;
    }
    else
    {
        ::warning(loc, "Floating point semantics for non-zero APFloat handled by converting to string and strtold");

        llvm::SmallString<16> Str;
        Val.toString(Str, 0, llvm::APFloat::semanticsPrecision(Val.getSemantics()));
        val = strtold(Str.c_str(), nullptr);
        t = Type::tfloat80;
    }

    if (pt) *pt = t;
    return new RealExp(loc, val, t);
}

Expression* ExprMapper::fromExpressionDeclRef(Loc loc, clang::NamedDecl* D,
                                    const clang::NestedNameSpecifier*, TypeQualifiedBuilderOpts tqualOpts)
{
    if (D->dsym) {
        if (auto d = D->dsym->isDeclaration()) {
            if (d->isFuncDeclaration() || d->isVarDeclaration())
                return new VarExp(loc, d);
            else if (auto td = d->isTemplateDeclaration())
                return new TemplateExp(loc, td);
            else if (auto t = d->getType())
                return new TypeExp(loc, t);
        } else if (auto em = D->dsym->isEnumMember())
            return em->getVarExp(loc, em->scope);
        // WARNING: implicit imports won't get added, should they be?
    }

    if (auto NTTP = dyn_cast<clang::NonTypeTemplateParmDecl>(D))
        return fromExpressionNonTypeTemplateParm(loc, NTTP);

    auto tqual = TypeMapper::FromType(tymap, loc).typeQualifiedFor(D, nullptr, nullptr, tqualOpts);
    assert(tqual && "DeclRefExpr decl without a DeclarationName");

    // Convert the TypeQualified path to DotXXXExp because
    // NOTE: they are preferable because unlike TypeExp, DotXXXExps call semantic() from left to right
    Expression *e = dotIdentOrInst(loc, nullptr, typeQualifiedRoot(tqual));
    for (auto id: tqual->idents)
        e = dotIdentOrInst(loc, e, id);

    return e;
}

template<typename T>
 Expression *ExprMapper::fromExpressionMemberExpr(Loc loc, const T* E, const clang::DeclarationName MemberName)
{
    assert(E->getBase() && "Unhandled case");

    auto member = getIdentOrTempinst(loc, MemberName, tymap);
    auto e1 = fromExpression(E->getBase());

    if (!e1 || !member)
        return nullptr;

    if (E->hasExplicitTemplateArgs())
    {
        assert(member->dyncast() == DYNCAST_IDENTIFIER);

        auto tempinst = new TemplateInstance(loc,
            static_cast<Identifier*>(member));
        tempinst->tiargs = fromASTTemplateArgumentListInfo(loc,
            E->getTemplateArgs(), E->getNumTemplateArgs(), tymap);

        member = tempinst;
    }

    if (auto NNS = E->getQualifier())
    {
        auto tqual = TypeMapper::FromType(tymap, loc).fromNestedNameSpecifier(NNS);
        e1 = dotIdentOrInst(loc, e1, typeQualifiedRoot(tqual));

        for (auto id : tqual->idents)
            e1 = dotIdentOrInst(loc, e1, id);
    }

    return dotIdentOrInst(loc, e1, member);
}

Expression* ExprMapper::fromExpressionNonTypeTemplateParm(Loc loc, const clang::NonTypeTemplateParmDecl* D)
{
    auto ident = DeclMapper::getIdentifierForTemplateNonTypeParm(D);
    return new IdentifierExp(loc, ident);
}

bool ExprMapper::toAPValue(clang::APValue& Result, Expression* e)
{
    using clang::APValue;

    auto& Context = calypso.getASTContext();

    if (e->op == TOKvar)
        e = e->optimize(WANTvalue);

    switch (e->op)
    {
        case TOKint64:
        {
            auto exp = static_cast<IntegerExp*>(e);
            auto value = exp->getInteger();

            auto IntType = tymap.toType(e->loc, e->type, nullptr);
            unsigned IntSize = Context.getTypeSize(IntType);

            llvm::APInt IntVal(IntSize, value);
            Result = APValue(llvm::APSInt(IntVal, exp->type->isunsigned()));

            return true;
        }
        case TOKfloat64:
        {
            auto exp = static_cast<RealExp *>(e);

            auto tofltSemantics = [] (Type *t) {
                switch(t->ty) {
                    case Tfloat32: return &llvm::APFloat::IEEEsingle;
                    case Tfloat64: return &llvm::APFloat::IEEEdouble;
                    default: llvm_unreachable("Unhandled tofltSemantics");
                }
            };

            llvm::APFloat Val(*tofltSemantics(exp->type));

            switch(exp->type->ty) {
                case Tfloat32:
                    Val = llvm::APFloat((float) exp->value);
                    break;
                case Tfloat64:
                    Val = llvm::APFloat((double) exp->value);
                    break;
                default:
                    llvm_unreachable("Unhandled float type");
            }

            Result = APValue(Val);
            return true;
        }
        case TOKstructliteral:
        {
            auto sle = static_cast<StructLiteralExp*>(e);

            unsigned elem_i = 0;
            std::function<bool(APValue &Val, AggregateDeclaration* ad)>
                toFields = [&] (APValue &Val, AggregateDeclaration* ad)
            {
                auto cd = ad->isClassDeclaration();

                unsigned numBases = 0;
                if (cd && cd->baseclasses)
                    numBases = cd->baseclasses->dim;

                Val = APValue(APValue::UninitStruct(),
                                        numBases, ad->fields.dim);

                if (auto base = toAggregateBase(ad))  // FIXME numBases > 1
                    if (!toFields(Val.getStructBase(0), base))
                        return false;

                for (unsigned i = 0; i < ad->fields.dim; i++, elem_i++)
                    if (!toAPValue(Val.getStructField(i), (*sle->elements)[elem_i]))
                        return false;

                return true;
            };

            if (!toFields(Result, sle->sd))
                return false;

            assert(elem_i == sle->elements->dim);
            return true;
        }
        default:
            return false;
    }
}

clang::Expr* ExprMapper::toExpression(Expression* e)
{
    auto& Context = calypso.getASTContext();
    clang::SourceLocation Loc;

    if (e->op == TOKvar)
        e = e->optimize(WANTvalue);

    switch (e->op)
    {
        case TOKint64:
        {
            auto exp = static_cast<IntegerExp*>(e);
            auto value = exp->getInteger();

            if (e->type->ty == Tenum)
            {
                auto ed = static_cast<TypeEnum*>(e->type)->sym;
                if (isCPP(ed))
                {
                    auto ED = static_cast<cpp::EnumDeclaration*>(ed)->ED;
                    
                    for (auto ECD: ED->enumerators())
                    {
                        auto& AI = ECD->getInitVal();
                        auto Val = AI.isSigned() ? AI.getSExtValue() : AI.getZExtValue();

                        if (Val == value)
                            return clang::DeclRefExpr::Create(Context,
                                clang::NestedNameSpecifierLoc(), clang::SourceLocation(),
                                ECD, false, Loc, Context.getEnumType(ED), clang::VK_RValue);
                    }
                }
            }

            if (e->type->ty == Tbool)
                return new (Context) clang::CXXBoolLiteralExpr(value != 0,
                                                               Context.BoolTy, Loc);

            unsigned IntSize = Context.getTargetInfo().getIntWidth();
            return clang::IntegerLiteral::Create(Context,
                                            llvm::APInt(IntSize, value),
                                            Context.IntTy, Loc);
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
