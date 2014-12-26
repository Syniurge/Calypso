// Contributed by Elie Morisse, same license DMD uses
#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppaggregate.h"
#include "cpp/cpptemplate.h"

#include "mtype.h"
#include "target.h"
#include "gen/dvalue.h"
#include "gen/functions.h"
#include "gen/logger.h"
#include "gen/irstate.h"
#include "ir/irfunction.h"
#include "gen/llvmhelpers.h"
#include "ir/irtype.h"
#include "ir/irtypeaggr.h"

#include "clang/CodeGen/CGFunctionInfo.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/lib/CodeGen/CGCXXABI.h"
#include "clang/lib/CodeGen/CGRecordLayout.h"
#include "clang/lib/CodeGen/CodeGenFunction.h"
#include "clang/lib/CodeGen/CodeGenTypes.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/Basic/ABI.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/ASTUnit.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

//////////////////////////////////////////////////////////////////////////////////////////

namespace cpp
{

void LangPlugin::enterModule(llvm::Module *m)
{
    if (!getASTUnit())
        return;

    auto& Context = getASTContext();

    clang::CodeGenOptions CGO;
    AB.reset(new AssistBuilder(*pch.Diags, m,
                                  CGO, llvm::getGlobalContext()));
    AB->Initialize(Context);
}

void LangPlugin::leaveModule()
{
    if (!getASTUnit())
        return;

    auto& Context = getASTContext();
    AB->HandleTranslationUnit(Context);
}

void LangPlugin::enterFunc(::FuncDeclaration *fd)
{//FIXME nested
    if (!getASTUnit())
        return;
    
    auto& CGM = *(AB->CGM());
    CGF = new clang::CodeGen::CodeGenFunction(CGM, true);
}

void LangPlugin::leaveFunc()
{
    if (!getASTUnit())
        return;
    
    delete CGF;
    CGF = nullptr;
}

void LangPlugin::updateCGFInsertPoint()
{
    auto BB = gIR->scope().begin;
    CGF->Builder.SetInsertPoint(BB);
}

llvm::Type *LangPlugin::toType(::Type *t)
{
    auto CGM = AB->CGM();
    auto& Context = getASTContext();

    switch(t->ty)
    {
        case Tstruct:
        {
            auto ts = static_cast<TypeStruct*>(t);
            auto c_sd = static_cast<StructDeclaration*>(ts->sym);
            return CGM->getTypes().ConvertTypeForMem(
                Context.getRecordType(c_sd->RD));
        }
        case Tclass:
        {
            auto tc = static_cast<TypeClass*>(t);
            auto c_cd = static_cast<ClassDeclaration*>(tc->sym);
            return CGM->getTypes().ConvertTypeForMem(
                Context.getRecordType(c_cd->RD));
        }
        default:
            assert(false && "Unhandled");
    }

    return nullptr;
}

llvm::Constant *LangPlugin::createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType)
{
    auto& Context = getASTContext();
    auto CGM = AB->CGM();

    const clang::RecordDecl *RD;
    if (auto sd = irAggr->aggrdecl->isStructDeclaration())
        RD = static_cast<StructDeclaration*>(sd)->RD;
    else if (auto cd = irAggr->aggrdecl->isClassDeclaration())
        RD = static_cast<ClassDeclaration*>(cd)->RD;
     else
        assert(false && "Unknown aggregate decl type?");

    auto DestType = Context.getRecordType(RD).withConst();
    llvm::Constant *c;

    // TODO: What we probably want is to have a default value if there's no constructor or a default trivial one,
    // but a null value if that's not the case i.e when context matters.
//     llvm::ArrayRef<clang::Expr*> initExprs;
//     auto ILE = new (Context) clang::InitListExpr(
//             Context, clang::SourceLocation(),
//             initExprs, clang::SourceLocation());
//     ILE->setType(DestType);
//
//     clang::Expr::EvalResult Result;
//     ILE->EvaluateAsLValue(Result, Context);

    c = CGM->EmitNullConstant(DestType);  // NOTE: neither EmitConstantExpr nor EmitConstantValue will work with CXXConstructExpr

    auto irSt = llvm::cast<llvm::StructType>(c->getType());

    if (initializerType)
    {
        assert(initializerType->isOpaque());
        initializerType->setBody(llvm::ArrayRef<llvm::Type*>(irSt->element_begin(),
                                                             irSt->element_end()),
                                                             irSt->isPacked());
    }
    
    c->mutateType(initializerType);  // dangerous?
    return c;
}

void LangPlugin::buildGEPIndices(IrTypeAggr *irTyAgrr,
                                 VarGEPIndices &varGEPIndices)
{
    auto& CGM = *AB->CGM();
    auto& CGTypes = CGM.getTypes();
    
    auto t = irTyAgrr->getDType();
    AggregateDeclaration *ad;
    const clang::RecordDecl *RD;
    
    if (t->ty == Tstruct)
    {
        auto ts = static_cast<TypeStruct*>(t);
        ad = ts->sym;
        RD = static_cast<cpp::StructDeclaration*>(ts->sym)->RD;
    }
    else if (t->ty == Tclass)
    {
        auto tc = static_cast<TypeClass*>(t);
        ad = tc->sym;
        RD = static_cast<cpp::ClassDeclaration*>(tc->sym)->RD;
    }
    
    auto& CGRL = CGTypes.getCGRecordLayout(RD);
    
    for (auto vd: ad->fields)
    {
        auto VD = static_cast<cpp::VarDeclaration*>(vd)->VD;
        varGEPIndices[vd] = CGRL.getLLVMFieldNo(llvm::cast<clang::FieldDecl>(VD));
    }
}

void LangPlugin::toInitClass(TypeClass* tc, LLValue* dst)
{
    auto& Context = getASTContext();
    auto& CGM = *AB->CGM();

    uint64_t const dataBytes = tc->sym->structsize;
    if (dataBytes == 0)
        return;

    LLValue* initsym = getIrAggr(tc->sym)->getInitSymbol();
    initsym = DtoBitCast(initsym, DtoType(tc));

    DtoMemCpy(dst, initsym, DtoConstSize_t(dataBytes));
}

LLValue *LangPlugin::toVirtualFunctionPointer(DValue* inst, 
                                              ::FuncDeclaration* fdecl, char* name)
{
    auto& CGM = *AB->CGM();
    auto c_fd = static_cast<FuncDeclaration*>(fdecl);
    
    auto MD = llvm::cast<const clang::CXXMethodDecl>(c_fd->FD);

    // get instance
    LLValue* vthis = inst->getRVal();

    const clang::CodeGen::CGFunctionInfo *FInfo = nullptr;
    if (const clang::CXXDestructorDecl *Dtor = llvm::dyn_cast<clang::CXXDestructorDecl>(MD))
        FInfo = &CGM.getTypes().arrangeCXXDestructor(Dtor,
                                                    clang::Dtor_Complete);
    else if (const clang::CXXConstructorDecl *Ctor = llvm::dyn_cast<clang::CXXConstructorDecl>(MD))
        FInfo = &CGM.getTypes().arrangeCXXConstructorDeclaration(Ctor,
                                                                clang::Ctor_Complete);
    else
        FInfo = &CGM.getTypes().arrangeCXXMethodDeclaration(MD);
  
    llvm::FunctionType *Ty = CGM.getTypes().GetFunctionType(*FInfo);
    
    return CGM.getCXXABI().getVirtualFunctionPointer(*CGF, MD, vthis, Ty);
}

DValue* LangPlugin::toCallFunction(Loc& loc, Type* resulttype, DValue* fnval, 
                                   Expressions* arguments, llvm::Value *retvar)
{
    auto& CGM = *AB->CGM();

    updateCGFInsertPoint();
    
    DFuncValue* dfnval = fnval->isFunc();
    auto c_fd = static_cast<FuncDeclaration*>(dfnval->func);

    // get function type info
    IrFuncTy &irFty = DtoIrTypeFunction(fnval);
    TypeFunction* tf = DtoTypeFunction(fnval);

    // get callee llvm value
    LLValue* callable = DtoCallableValue(fnval);
    LLFunctionType* callableTy = DtoExtractFunctionType(callable->getType());
    assert(callableTy);

    clang::CodeGen::RValue RV;
    clang::CodeGen::ReturnValueSlot ReturnValue(retvar, false);
    clang::CodeGen::CallArgList Args;

    auto FD = c_fd->FD;
    auto MD = llvm::dyn_cast<const clang::CXXMethodDecl>(FD);

    auto This = MD ? dfnval->vthis : nullptr;

    const clang::FunctionProtoType *FPT = FD->getType()->castAs<clang::FunctionProtoType>();

    auto Dtor = llvm::dyn_cast<clang::CXXDestructorDecl>(FD);
    if (Dtor && Dtor->isVirtual())
    {
        CGM.getCXXABI().EmitVirtualDestructorCall(*CGF, Dtor, clang::Dtor_Complete,
                                        clang::SourceLocation(), This);

        return new DImValue(nullptr, nullptr); // WARNING ldc never does that, it returns the instruction of the call site instead
    }

    // Push the this ptr.
    if (MD && !MD->isStatic())
    {
        if (MD->isVirtual()) {
            This = CGM.getCXXABI().adjustThisArgumentForVirtualFunctionCall(
                *CGF, MD, This, true);
        }

        Args.add(clang::CodeGen::RValue::get(This),
                 MD->getThisType(getASTContext()));
    }

    size_t n = Parameter::dim(tf->parameters);
    for (size_t i=0; i<n; ++i) {
        Parameter* fnarg = Parameter::getNth(tf->parameters, i);
        assert(fnarg);
        DValue* argval = DtoArgument(fnarg,
                        static_cast<::Expression*>(arguments->data[i]));
        Args.add(clang::CodeGen::RValue::get(argval->getRVal()),
                 fromType(fnarg->type));
    }
    
    if (MD && !MD->isStatic())
    {
        clang::CodeGen::RequiredArgs required =
            clang::CodeGen::RequiredArgs::forPrototypePlus(FPT, Args.size());
        
        RV = CGF->EmitCall(CGM.getTypes().arrangeCXXMethodCall(Args, FPT, required),
                    callable, ReturnValue, Args, MD);
    }
    else
    {
        RV = CGF->EmitCall(CGM.getTypes().arrangeFreeFunctionCall(Args, FPT),
                    callable, ReturnValue, Args, FD);
    }

    return new DImValue(resulttype, RV.getScalarVal());
}

// IMPORTANT NOTE: Clang emits forward decls lazily, which means that no function prototype is actually emitted to LLVM after making a CodeGenerator consume the ASTContext.
// One reason for that is that types need to be complete for a prototype to be emitted (this used to cause obscure segfaults on my language Tales a while back).
// This is why we must keep access to the CodeGenModule so that we can emit them and then copy the LLVM infos.
// (as a side not similarly LLVM lazily puts aggregate types into its context type map as well, but that doesn't matter here afaik).

void LangPlugin::toResolveFunction(::FuncDeclaration* fdecl)
{
    auto CGM = AB->CGM();
    auto FD = static_cast<FuncDeclaration*>(fdecl)->FD;

    DtoFunctionType(fdecl);

    fdecl->ir.setDeclared();

    llvm::Constant *Callee = nullptr;
    const clang::CodeGen::CGFunctionInfo *FInfo;
    llvm::FunctionType *Ty;

    if (auto MD = llvm::dyn_cast<const clang::CXXMethodDecl>(FD))
    {
        if (auto Dtor = llvm::dyn_cast<const clang::CXXDestructorDecl>(MD))
            FInfo = &CGM->getTypes().arrangeCXXDestructor(Dtor,
                                                        clang::Dtor_Complete);
        else if (auto Ctor = llvm::dyn_cast<const clang::CXXConstructorDecl>(MD))
            FInfo = &CGM->getTypes().arrangeCXXConstructorDeclaration(Ctor,
                                                                    clang::Ctor_Complete);
        else
            FInfo = &CGM->getTypes().arrangeCXXMethodDeclaration(MD);

        Ty = CGM->getTypes().GetFunctionType(*FInfo);
    }
    else
        Ty = CGM->getTypes().GetFunctionType(FD);


    if (auto Dtor = llvm::dyn_cast<const clang::CXXDestructorDecl>(FD))
        Callee = CGM->GetAddrOfCXXDestructor(Dtor, clang::Dtor_Complete,
                                                FInfo, Ty);
    else if (auto Ctor = llvm::dyn_cast<const clang::CXXConstructorDecl>(FD))
        Callee = CGM->GetAddrOfFunction(
            clang::GlobalDecl(Ctor, clang::Ctor_Complete), Ty);
    else
        Callee = CGM->GetAddrOfFunction(FD, Ty);

    auto irFunc = getIrFunc(fdecl, true);
    irFunc->func = llvm::cast<llvm::Function>(Callee);
}

void LangPlugin::addBaseClassData(AggrTypeBuilder &b, ::AggregateDeclaration *base)
{
    auto& CGM = *AB->CGM();

    const clang::RecordDecl *RD;
    if (base->isClassDeclaration())
        RD = static_cast<cpp::ClassDeclaration*>(base)->RD;
    else
        RD  = static_cast<cpp::StructDeclaration*>(base)->RD;

    auto& CGRL = CGM.getTypes().getCGRecordLayout(RD);

    auto BaseTy = CGRL.getBaseSubobjectLLVMType();
    auto BaseLayout = gDataLayout->getStructLayout(BaseTy);

    for (auto I = BaseTy->element_begin(), E = BaseTy->element_end();
         I != E; ++I)
    {
        b.m_defaultTypes.push_back(*I);
        ++b.m_fieldIndex;
    }

    for (auto vd: base->fields)
    {
        auto VD = static_cast<cpp::VarDeclaration*>(vd)->VD;
        b.m_varGEPIndices[vd] = CGRL.getLLVMFieldNo(llvm::cast<clang::FieldDecl>(VD));
    }

    b.m_offset += BaseLayout->getSizeInBytes();
}

void LangPlugin::toDeclareVariable(::VarDeclaration* vd)
{
    auto& CGM = *AB->CGM();
    auto VD = llvm::cast<clang::VarDecl>(
            static_cast<cpp::VarDeclaration*>(vd)->VD);

//     updateCGFInsertPoint();

    LLValue *v;

    // If it's thread_local, emit a call to its wrapper function instead.
//     if (VD->getTLSKind() == clang::VarDecl::TLS_Dynamic)
//         v = CGM.getCXXABI().EmitThreadLocalVarDeclLValue(*CGF, VD, VD->getType()).getAddress();
//     else
        v = CGM.GetAddrOfGlobalVar(VD);

    getIrGlobal(vd)->value = v;
}

void LangPlugin::toDefineTemplateInstance(::TemplateInstance *inst)
{
    auto c_inst = static_cast<cpp::TemplateInstance *>(inst);

    if (c_inst->instantiatingModuleCpp == gIR->dmodule) // hmm this just works but the whole thing still is a fragile HACK to avoid duplicate instances
    {
        auto CTSD = llvm::cast<clang::ClassTemplateSpecializationDecl>(c_inst->Instantiated);
        AB->HandleTagDeclDefinition(CTSD);
    }
}

}
