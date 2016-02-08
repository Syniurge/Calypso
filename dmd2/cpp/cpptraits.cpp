// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "scope.h"
#include "template.h"

#include "clang/AST/VTableBuilder.h"

namespace cpp
{

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

Expression *LangPlugin::semanticTraits(TraitsExp *e, Scope *sc)
{
    if (!pch.AST)
    {
        e->error("C++ traits without any C++ module");
        fatal();
    }

    auto& Context = getASTContext();
    size_t dim = e->args ? e->args->dim : 0;

    if (e->ident == Identifier::idPool("isCpp"))
    {
        if (dim != 1)
            goto Ldimerror;
        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        if (!s) { }
        else if (isCPP(s))
            goto Ltrue;
        else
            goto Lfalse;

        e->error("symbol expected instead of '%s'", o->toChars());
        goto Lfalse;
    }
    else if (e->ident == Identifier::idPool("getCppVirtualIndex"))
    {
        if (dim != 1)
            goto Ldimerror;
        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        ::FuncDeclaration *fd;

        assert(s->toParent()->isClassDeclaration());
        auto cd = static_cast<::ClassDeclaration*>(s->toParent());
        if (auto cxxparent = isDCXX(cd))
            cd = cxxparent;

        if (!s || (fd = s->isFuncDeclaration()) == NULL || !isCPP(cd))
        {
//             e->error("first argument to __traits(getCppVirtualIndex) must be a C++ virtual method or a D method overriding a C++ virtual method");
            return new IntegerExp(e->loc, -1, Type::tptrdiff_t);
        }
        fd = fd->toAliasFunc(); // Neccessary to support multiple overloads.

        if (!isCPP(fd))
            fd = findOverriddenMethod(fd, cd);
        if (!fd || !isCPP(fd) || !fd->isVirtualMethod())
            return new IntegerExp(e->loc, -1, Type::tptrdiff_t);
        auto c_fd = static_cast<cpp::FuncDeclaration*>(fd);
        auto MD = cast<clang::CXXMethodDecl>(c_fd->FD);

        assert(!Context.getVTableContext()->isMicrosoft() && "Not implemented yet");
        auto& VTableContext = *static_cast<clang::ItaniumVTableContext*>(Context.getVTableContext());

        uint64_t Index = VTableContext.getMethodVTableIndex(MD);
        auto PointerWidth = Context.toCharUnitsFromBits(Context.getTargetInfo().getPointerWidth(0));
        uint64_t VTableOffset = Index * PointerWidth.getQuantity();

        return new IntegerExp(e->loc, VTableOffset, Type::tptrdiff_t);
    }
    else if (e->ident == Identifier::idPool("getBaseOffset"))
    {
        if (dim != 2)
            goto Ldimerror;
        RootObject *o1 = (*e->args)[0];
        RootObject *o2 = (*e->args)[1];
        auto t1 = getType(o1)->toBasetype();
        auto t2 = getType(o2)->toBasetype();

        int offset = 0;
        if (!t1 || !t2 || (t1 != t2 && !t2->isBaseOf(t1, &offset)))
            return new IntegerExp(e->loc, -1, Type::tptrdiff_t);

        return new IntegerExp(e->loc, offset, Type::tptrdiff_t);
    }
    assert(0);

Ldimerror:
    e->error("wrong number of arguments %d", (int)dim);

Lfalse:
    return new IntegerExp(e->loc, 0, Type::tbool);

Ltrue:
    return new IntegerExp(e->loc, 1, Type::tbool);
}

}