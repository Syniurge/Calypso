// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPEXPRESSION_H
#define DMD_CPP_CPPEXPRESSION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "expression.h"
#include "cpp/calypso.h"
#include "cpp/cpptypes.h"

namespace llvm
{
class APSInt;
}

namespace clang
{
class Expr;
}

namespace cpp
{

class ExprMapper
{
public:
    ExprMapper(TypeMapper &tymap)
        : tymap(tymap) {}

    // Clang -> DMD
    Expression *fromExpression(const clang::Expr *E, bool interpret = false);

    Expression *fromExpressionDeclRef(Loc loc, clang::NamedDecl* D,
                                    const clang::NestedNameSpecifier* NNS = nullptr,
                                    TypeQualifiedBuilderOpts tqualOpts = TQ_OverOpFullIdent);
    Expression *fromExpressionNonTypeTemplateParm(Loc loc,
                                    const clang::NonTypeTemplateParmDecl* D);

    Expression *fromAPValue(Loc loc, const clang::APValue &Val, clang::QualType Ty = clang::QualType());
    Expression *fromAPInt(Loc loc, const llvm::APSInt &Val, clang::QualType Ty = clang::QualType());
    Expression *fromAPFloat(Loc loc, const APFloat &Val, Type **pt = nullptr);

    Expression *fixIntegerExp(IntegerExp *e, clang::QualType T); // revert integer literals to DeclRefs pointing to enum constants if T is an EnumType

    // DMD -> Clang
    clang::Expr *toExpression(Expression *e);

protected:
    TypeMapper &tymap;

    Expression* fromUnaExp(clang::SourceLocation Loc, const clang::UnaryOperator::Opcode Op,
                           const clang::Expr *SubExpr);
    Expression* fromBinExp(clang::SourceLocation Loc, const clang::BinaryOperator::Opcode Op,
                           const clang::Expr *LHS, const clang::Expr *RHS);
    Expression* fromUnaExp(const clang::UnaryOperator *E);
    Expression* fromBinExp(const clang::BinaryOperator* E);

    Expression *fromCastExpr(Loc loc, const clang::CastExpr *E);
};

}

#endif