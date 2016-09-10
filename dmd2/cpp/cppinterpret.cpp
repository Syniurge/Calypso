// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

bool LangPlugin::canInterpret(::FuncDeclaration *fd)
{
    auto FD = getFD(fd);
    return FD->isConstexpr() && FD->hasBody();
}

Expression *LangPlugin::interpret(::FuncDeclaration *fd, InterState *istate, Expressions *arguments,
                                  Expression *thisarg)
{
    auto& Context = calypso.getASTContext();
    auto FD = const_cast<clang::FunctionDecl*>(getFD(fd));

    TypeMapper tymap;
    ExprMapper expmap(tymap);
    tymap.addImplicitDecls = false;

    llvm::SmallVector<clang::Expr*, 2> Args;
    for (auto arg: *arguments) {
        auto E = expmap.toExpression(arg);
        assert(E);
        Args.push_back(E);
    }

    Expression *r;

    auto Evaluate = [&] (clang::Expr* E, Expression **result = nullptr) {
        clang::Expr::EvalResult Result;
        auto b = E->EvaluateAsRValue(Result, Context);

        Loc loc;
        return expmap.fromAPValue(loc, Result.Val, clang::QualType(), result);
    };

    if (auto CCD = dyn_cast<clang::CXXConstructorDecl>(FD)) {
        assert(thisarg);
        auto RD = CCD->getParent();

        auto Construct = clang::CXXConstructExpr::Create(Context, Context.getRecordType(RD).withConst(),
                                clang::SourceLocation(), CCD, false, Args, false, false, false, false,
                                clang::CXXConstructExpr::CK_Complete, clang::SourceLocation());

        r = Evaluate(Construct, &thisarg);
    } else {
        auto DeclRef = clang::DeclRefExpr::Create(Context, clang::NestedNameSpecifierLoc(),
                                clang::SourceLocation(), FD, false, clang::SourceLocation(), FD->getType(),
                                clang::VK_LValue);
        auto Cast = clang::ImplicitCastExpr::Create(Context, Context.getPointerType(DeclRef->getType()),
                                    clang::CK_FunctionToPointerDecay, DeclRef, nullptr, clang::VK_RValue);
        auto Call = new (Context) clang::CallExpr(Context, Cast, Args, FD->getReturnType(),
                                clang::VK_RValue, clang::SourceLocation());

        r = Evaluate(Call);

        // NOTE: clang::Stmt cannot be deleted, they can only be freed by ASTContext's dtor
    }

    assert(r);
    return r;
}

}
