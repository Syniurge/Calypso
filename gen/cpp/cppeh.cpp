// Contributed by Elie Morisse, same license DMD uses
#include "cpp/calypso.h"
#include "cpp/cppstatement.h"
#include "cpp/cpptypes.h"

#include "ir/irfunction.h"
#include "ir/irtypeclass.h"
#include "gen/irstate.h"
#include "gen/llvmhelpers.h"
#include "gen/rttibuilder.h"
#include "gen/tollvm.h"

#include "clang/lib/CodeGen/Address.h"
#include "clang/lib/CodeGen/CGCXXABI.h"
#include "clang/lib/CodeGen/CGException.h"
#include "clang/lib/CodeGen/CodeGenFunction.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

namespace clangCG = clang::CodeGen;

struct IrCatch
{
    clangCG::CodeGenFunction::RunCleanupsScope *CatchScope = nullptr; // may be overkill
};

IrCatch *getIrCatch(cpp::Catch *cj)
{
    if (!cj->ir)
        cj->ir = new IrCatch;
    return cj->ir;
}

void LangPlugin::toBeginCatch(IRState *irs, ::Catch *cj)
{
    auto c_cj = static_cast<cpp::Catch*>(cj);
    auto irc = getIrCatch(c_cj);
    llvm::Value* ehPtr = irs->func()->getOrCreateEhPtrSlot();
    ehPtr = irs->ir->CreateLoad(ehPtr);

    updateCGFInsertPoint();

    // Enter a cleanup scope, including the catch variable and the
    // end-catch.
    irc->CatchScope = new clangCG::CodeGenFunction::RunCleanupsScope(*CGF());

    // For catches handling a specific type, create storage for it.
    // We will set it in the code that branches from the landing pads
    // (there might be more than one) to catchBB.
    if (cj->var) {
//         // Use the same storage for all exceptions that are not accessed in
//         // nested functions
//         if (!cj->var->nestedrefs.dim) {
//             assert(!isIrLocalCreated(cj->var));
//             IrLocal* irLocal = getIrLocal(cj->var, true);
//             irLocal->value = DtoBitCast(ehPtr, getPtrToType(llCatchVarType));
//         } else {
            // This will alloca if we haven't already and take care of nested refs
            DtoDeclarationExp(cj->var);
            IrLocal* irLocal = getIrLocal(cj->var);

            auto CatchParamTy = TypeMapper().toType(cj->loc, cj->var->type,
                                                        irs->func()->decl->scope, cj->var->storage_class);
            clangCG::Address ParamAddr(irLocal->value,
                                       CGF()->getNaturalTypeAlignment(CatchParamTy));

            clangCG::InitCatchParam(*CGF(), ehPtr, CatchParamTy,
                                    nullptr, ParamAddr, clang::SourceLocation());
//         }
    }
    else
        clangCG::CallBeginCatch(*CGF(), ehPtr, false);
}

void LangPlugin::toEndCatch(IRState *irs, ::Catch *cj)
{
    auto c_cj = static_cast<cpp::Catch*>(cj);

    updateCGFInsertPoint();

    // Call __cxa_end_catch, fall through the catch cleanups.
    c_cj->ir->CatchScope->ForceCleanup();
    delete c_cj->ir->CatchScope;
}

llvm::Constant *LangPlugin::toCatchScopeType(IRState *irs, Type *t)
{
    auto loc = irs->func()->decl->loc;
    auto ThrowType = TypeMapper().toType(loc, t, irs->func()->decl->scope);

    auto TypeInfo = CGM->GetAddrOfRTTIDescriptor(ThrowType, /*ForEH=*/true);
    auto& wrapper = type_infoWrappers[TypeInfo];

    if (!wrapper)
    {
        if (!type_info_ptr) {
			Identifiers packages;
			packages.push(Identifier::idPool("cpp"));
			auto dst = Package::resolve(&packages, nullptr, nullptr);
			auto cpp_core_module = dst->lookup(Identifier::idPool("core"))->isModule();
			
            type_info_ptr = cpp_core_module->search(
                        loc, Identifier::idPool("__cpp_type_info_ptr"))->isClassDeclaration();
		}

        assert(type_info_ptr);

        RTTIBuilder b(type_info_ptr);
        b.push(TypeInfo);

        auto initType = cast<llvm::StructType>(static_cast<IrTypeClass*>(type_info_ptr->type->ctype)->getMemoryLLType());
        auto finalinit = b.get_constant(initType);

        llvm::SmallString<256> InitName("_D");
        llvm::raw_svector_ostream Out(InitName);
        CGM->getCXXABI().getMangleContext().mangleCXXRTTI(ThrowType, Out);
        InitName.append("7__tiwrap");

        wrapper = getOrCreateGlobal(irs->func()->decl->loc,
            gIR->module, initType, true, llvm::GlobalValue::LinkOnceODRLinkage, finalinit, InitName);
        wrapper->setAlignment(DtoAlignment(type_info_ptr->type));
    }

    return wrapper;
}

}

