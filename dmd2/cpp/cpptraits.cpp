// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "cpp/calypso.h"
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

    if (e->ident == Identifier::idPool("getCppVirtualIndex"))
    {
        if (dim != 1)
            goto Ldimerror;
        RootObject *o = (*e->args)[0];
        Dsymbol *s = getDsymbol(o);
        ::FuncDeclaration *fd;
        if (!s || (fd = s->isFuncDeclaration()) == NULL || !isCPP(fd))
        {
            e->error("first argument to __traits(getCppVirtualIndex) must be a C++ function");
            goto Lfalse;
        }
        fd = fd->toAliasFunc(); // Neccessary to support multiple overloads.

        auto c_fd = static_cast<cpp::FuncDeclaration*>(fd);
        auto MD = cast<clang::CXXMethodDecl>(c_fd->FD);

        assert(!Context.getVTableContext()->isMicrosoft() && "Not implemented yet");
        auto& VTableContext = *static_cast<clang::ItaniumVTableContext*>(Context.getVTableContext());

        uint64_t Index = VTableContext.getMethodVTableIndex(MD);
        auto PointerWidth = Context.toCharUnitsFromBits(Context.getTargetInfo().getPointerWidth(0));
        uint64_t VTableOffset = Index * PointerWidth.getQuantity();

        return new IntegerExp(e->loc, VTableOffset, Type::tptrdiff_t);
    }
    assert(0);

Ldimerror:
    e->error("wrong number of arguments %d", (int)dim);

Lfalse:
    return new IntegerExp(e->loc, 0, Type::tbool);
}

}