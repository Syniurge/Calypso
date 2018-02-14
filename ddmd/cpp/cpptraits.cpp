// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/ddmdstructor.h"
#include "scope.h"
#include "template.h"

#include "clang/AST/VTableBuilder.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaDiagnostic.h"

namespace cpp
{

using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

Expression *LangPlugin::semanticTraits(TraitsExp *e, Scope *sc)
{
    if (e->ident != id_isCpp && e->ident != id_getCppVirtualIndex && 
        e->ident != id_getBaseOffset && e->ident != id_getMemberPointerExtraSlots)
        return nullptr;

    if (!pch.AST)
    {
        e->error("C++ traits without any C++ module");
        fatal();
    }

    auto& Context = getASTContext();
    auto& S = getSema();
    auto& Diags = getDiagnostics();
    size_t dim = e->args ? e->args->dim : 0;

    if (e->ident == id_isCpp)
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
    else if (e->ident == id_getCppVirtualIndex)
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
            return new_IntegerExp(e->loc, -1, Type::tptrdiff_t);
        }
        fd = fd->toAliasFunc(); // Neccessary to support multiple overloads.

        if (!isCPP(fd))
            fd = findOverriddenMethod(fd, cd);
        if (!fd || !isCPP(fd) || !fd->isVirtualMethod())
            return new_IntegerExp(e->loc, -1, Type::tptrdiff_t);
        auto c_fd = static_cast<cpp::FuncDeclaration*>(fd);
        auto MD = cast<clang::CXXMethodDecl>(c_fd->FD);

        assert(!Context.getVTableContext()->isMicrosoft() && "Not implemented yet");
        auto& VTableContext = *static_cast<clang::ItaniumVTableContext*>(Context.getVTableContext());

        uint64_t Index = VTableContext.getMethodVTableIndex(MD);
        auto PointerWidth = Context.toCharUnitsFromBits(Context.getTargetInfo().getPointerWidth(0));
        uint64_t VTableOffset = Index * PointerWidth.getQuantity();

        return new_IntegerExp(e->loc, VTableOffset, Type::tptrdiff_t);
    }
    else if (e->ident == id_getBaseOffset)
    {
        if (dim != 2)
            goto Ldimerror;
        RootObject *o1 = (*e->args)[0];
        RootObject *o2 = (*e->args)[1];
        auto t1 = getType(o1)->toBasetype();
        auto t2 = getType(o2)->toBasetype();

        int offset = 0;
        if (!t1 || !t2 || (t1 != t2 && !t2->isBaseOf(t1, &offset)))
            return new_IntegerExp(e->loc, -1, Type::tptrdiff_t);

        return new_IntegerExp(e->loc, offset, Type::tptrdiff_t);
    }
    else if (e->ident == id_getMemberPointerExtraSlots)
    {
        if (dim != 1)
            goto Ldimerror;
        RootObject *o = (*e->args)[0];
        Type *t = getType(o)->toBasetype();

        Expressions *exps = new Expressions;
        
        // In the Microsoft ABI the number of fields of a member pointer type depends on the record type
        if (Context.getTargetInfo().getCXXABI().isMicrosoft()) {
            auto RD = cast<clang::CXXRecordDecl>(getRecordDecl(t));
            auto Class = Context.getRecordType(RD).getTypePtr();
            auto MPT = Context.getMemberPointerType(Context.VoidPtrTy, Class);
            assert(!MPT->isDependentType());

            if (!S.RequireCompleteType(RD->getLocation(), MPT, clang::diag::err_incomplete_type)) {
                auto Inheritance = MPT->castAs<clang::MemberPointerType>()
                        ->getMostRecentCXXRecordDecl()->getMSInheritanceModel();
                exps->push(new_IntegerExp(e->loc,
                    clang::MSInheritanceAttr::hasNVOffsetField(true, Inheritance) ? 1 : 0, Type::tbool));
                exps->push(new_IntegerExp(e->loc,
                    clang::MSInheritanceAttr::hasVBPtrOffsetField(Inheritance) ? 1 : 0, Type::tbool));
                exps->push(new_IntegerExp(e->loc,
                    clang::MSInheritanceAttr::hasVBTableOffsetField(Inheritance) ? 1 : 0, Type::tbool));
            } else
                Diags.Reset();
        }

        if (!exps->dim)
            for (unsigned i = 0; i < 3; i++)
                exps->push(new_IntegerExp(e->loc, 0, Type::tbool));

        // FIXME: No idea why the following code:
        //   enum extraSlots = [ __traits(getMemberPointerExtraSlots, Cls) ];
        // results in "Error: cannot resolve type for tuple(true, true, true)"
        // So returning an ArrayLiteralExp instead of a TupleExp for now..

        Expression* ex = new_ArrayLiteralExp(e->loc, exps);
        return semantic(ex, sc);
    }
    assert(0);

Ldimerror:
    e->error("wrong number of arguments %d", (int)dim);

Lfalse:
    return new_IntegerExp(e->loc, 0, Type::tbool);

Ltrue:
    return new_IntegerExp(e->loc, 1, Type::tbool);
}

}
