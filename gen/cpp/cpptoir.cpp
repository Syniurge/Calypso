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
#include "gen/classes.h"
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
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/ABI.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Lookup.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include <memory>

//////////////////////////////////////////////////////////////////////////////////////////

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

namespace clangCG = clang::CodeGen;

void LangPlugin::enterModule(::Module *, llvm::Module *lm)
{
    if (!getASTUnit())
        return;

    pch.save(); // save the numerous instantiations done by DMD back into the PCH

    auto& Context = getASTContext();

    auto Opts = new clang::CodeGenOptions;
    if (global.params.symdebug)
        Opts->setDebugInfo(clang::CodeGenOptions::FullDebugInfo);

    CGM.reset(new clangCG::CodeGenModule(Context,
                            *Opts, *lm, *gDataLayout, *pch.Diags));
    type_infoWrappers.clear();
    EmittedStaticVars.clear();
}

void removeDuplicateModuleFlags(llvm::Module *lm)
{
    auto ModFlags = lm->getModuleFlagsMetadata();
    if (!ModFlags)
        return;

    // Before erasing, check that module flags with the same name are the same value
    for (unsigned I = 0; I < ModFlags->getNumOperands(); I++)
        for (unsigned J = 0; J < ModFlags->getNumOperands(); J++)
    {
        if (I == J)
            continue;

        auto Op1 = ModFlags->getOperand(I);
        auto Op2 = ModFlags->getOperand(J);
        auto ID1 = cast<llvm::MDString>(Op1->getOperand(1));
        auto ID2 = cast<llvm::MDString>(Op2->getOperand(1));

        if (Op1 != Op2 &&
                ID1->getString().compare(ID2->getString()) == 0)
        {
            ::error(Loc(), "Two module flags named '%s', yet different values", ID1->getString().str().c_str());
            fatal();
        }
    }

    llvm::SmallVector<llvm::Module::ModuleFlagEntry, 3> Flags;
    lm->getModuleFlagsMetadata(Flags);
    ModFlags->dropAllReferences();

    llvm::DenseSet<const llvm::MDString *> SeenIDs;
    for (auto Flag: Flags)
    {
        if (SeenIDs.count(Flag.Key))
            continue;
        SeenIDs.insert(Flag.Key);

        lm->addModuleFlag(Flag.Behavior, Flag.Key->getString(), Flag.Val);
    }
}

void LangPlugin::leaveModule(::Module *m, llvm::Module *lm)
{
    if (!getASTUnit())
        return;

    // HACK temporarily rename the @llvm.global_ctors and @llvm.global_dtors variables created by LDC,
    // because CodeGenModule::Release will assume that they do not exist and use the same name, which LLVM will change to an unused one.
    auto ldcCtor = lm->getNamedGlobal("llvm.global_ctors"),
        ldcDtor = lm->getNamedGlobal("llvm.global_dtors");

    if (ldcCtor) ldcCtor->setName("llvm.global_ctors__d");
    if (ldcDtor) ldcDtor->setName("llvm.global_dtors__d");

    CGM->Release();

    // Then swap them back and append the Clang global structors to the LDC ones.
    // NOTE: the Clang created ones have a slightly different struct type, with an additional "key" that may be null or used for COMDAT stuff
    auto clangCtor = lm->getNamedGlobal("llvm.global_ctors"),
        clangDtor = lm->getNamedGlobal("llvm.global_dtors");

    auto MergeGlobalStors = [&] (llvm::GlobalVariable *ldcStor, llvm::GlobalVariable *clangStor,
                          const char *ArrayName, decltype(llvm::appendToGlobalCtors) &appendToGlobalStors)
    {
        if (!ldcStor)
            return;
        
        if (!clangStor)
        {
            ldcStor->setName(ArrayName);
            return;
        }

        std::string suffix("__c");

        clangStor->setName(llvm::Twine(ArrayName, suffix));
        ldcStor->setName(ArrayName);

        if (auto Init = clangCtor->getInitializer())
        {
            unsigned n = Init->getNumOperands();
            for (unsigned i = 0; i != n; ++i)
            {
                auto Stor = cast<llvm::User>(Init->getOperand(i));

                auto Priority = static_cast<uint32_t>(
                        cast<llvm::ConstantInt>(Stor->getOperand(0))->getSExtValue());
                auto Fn = cast<llvm::Function>(Stor->getOperand(1));
                appendToGlobalStors(*lm, Fn, Priority);
            }
        }

        clangStor->eraseFromParent();
    };

    MergeGlobalStors(ldcCtor, clangCtor, "llvm.global_ctors", llvm::appendToGlobalCtors);
    MergeGlobalStors(ldcDtor, clangDtor, "llvm.global_dtors", llvm::appendToGlobalDtors);

    // HACK to set the linkage of static variables to External, D modules being more fragmented than C++ translation unit
    // This is done here since most decls are emitted by CodeGenModule::Release()
    for (auto VD: EmittedStaticVars)
    {
        auto GV = cast<llvm::GlobalValue>(CGM->GetAddrOfGlobalVar(VD));
        GV->setLinkage(llvm::GlobalValue::ExternalLinkage);
    }

    // HACK Check and remove duplicate module flags such as "Debug Info Version" created by both Clang and LDC
    removeDuplicateModuleFlags(lm);

    CGM.reset();

    if (!global.errors && isCPP(m))
        calypso.genModSet.add(m);
}

void LangPlugin::enterFunc(::FuncDeclaration *fd)
{
    if (!getASTUnit())
        return;

    IrFunction *irFunc = getIrFunc(fd);

    CGFStack.push(new clangCG::CodeGenFunction(*CGM, true));
    CGF()->CurCodeDecl = nullptr;
    CGF()->AllocaInsertPt = irFunc->allocapoint;
}

void LangPlugin::leaveFunc()
{
    if (!getASTUnit())
        return;
    
    CGF()->AllocaInsertPt = nullptr;
    delete CGF();
    CGFStack.pop();
}

void LangPlugin::updateCGFInsertPoint()
{
    auto BB = gIR->scope().begin;
    CGF()->Builder.SetInsertPoint(BB);
}

struct ResolvedFunc
{
    llvm::Function *Func = nullptr;
    llvm::FunctionType *Ty = nullptr;

    static bool isIncompleteTagType(const clang::QualType T)
    {
        return T->isIncompleteType() && !T->isBuiltinType();
    }

    static ResolvedFunc get(clangCG::CodeGenModule &CGM, const clang::FunctionDecl *FD)
    {
        ResolvedFunc result;
        const clangCG::CGFunctionInfo *FInfo;

        // If there's an incomplete type, don't declare it
        if (isIncompleteTagType(FD->getReturnType()))
            return result;

        for (auto& Param: FD->params())
            if (isIncompleteTagType(Param->getType()))
                return result;

        auto MD = dyn_cast<const clang::CXXMethodDecl>(FD);

        clangCG::StructorType structorType = clangCG::StructorType::Complete;
        // Most of the time we only need the complete ***structor, but for abstract classes there simply isn't one
        if (MD && MD->getParent()->isAbstract())
            structorType = clangCG::StructorType::Base;

        if (MD)
        {
            if (isa<const clang::CXXConstructorDecl>(FD) || isa<const clang::CXXDestructorDecl>(FD))
                FInfo = &CGM.getTypes().arrangeCXXStructorDeclaration(MD, structorType);
            else
                FInfo = &CGM.getTypes().arrangeCXXMethodDeclaration(MD);

            result.Ty = CGM.getTypes().GetFunctionType(*FInfo);
        }
        else
            result.Ty = CGM.getTypes().GetFunctionType(FD);

        llvm::Constant *GV;
        if (isa<const clang::CXXConstructorDecl>(FD) || isa<const clang::CXXDestructorDecl>(FD))
            GV = CGM.getAddrOfCXXStructor(MD, structorType, FInfo, result.Ty, true);
        else
            GV = CGM.GetAddrOfFunction(FD, result.Ty, false, true); // NOTE: DontDefer needs to be true or else many functions will get wrongly emitted in this module (sometimes causing linking errors)
        result.Func = cast<llvm::Function>(GV);

        auto FPT = FD->getType()->getAs<clang::FunctionProtoType>();
        const clang::FunctionDecl *Def;

        // If this is a static or always inlined function, emit it in any module calling or referencing it
        if (result.Func->isDeclaration() && FD->hasBody(Def) &&
                FPT->getExceptionSpecType() != clang::EST_Unevaluated)
            if (!FD->hasExternalFormalLinkage() ||
                    FD->hasAttr<clang::AlwaysInlineAttr>())
                CGM.EmitTopLevelDecl(const_cast<clang::FunctionDecl*>(Def));

        return result;
    }
};

llvm::Type *LangPlugin::toType(::Type *t)
{
    auto& Context = getASTContext();

    auto RD = getRecordDecl(t);

    if (RD->isInvalidDecl() || !RD->getDefinition())
        return llvm::StructType::get(gIR->context());
    else
        return CGM->getTypes().ConvertTypeForMem(
                    Context.getRecordType(RD));
}

llvm::FunctionType *LangPlugin::toFunctionType(::FuncDeclaration *fdecl)
{
    auto irFunc = getIrFunc(fdecl, true);

    auto FD = getFD(fdecl);
    auto Resolved = ResolvedFunc::get(*CGM, FD);

    irFunc->irFty.funcType = Resolved.Ty;
    return Resolved.Ty;
}

llvm::Type *LangPlugin::IrTypeStructHijack(::StructDeclaration *sd) // HACK but if we don't do this LLVM type comparisons will fail
{
    if (sd->ident == Identifier::idPool("__cpp_member_funcptr"))
    {
        auto Ty = TypeMapper().toType(Loc(), sd->getType(), nullptr);

        if (auto MPT = Ty->getAs<clang::MemberPointerType>())
            return CGM->getCXXABI().ConvertMemberPointerType(MPT);
    }

    return nullptr;
}

LLConstant *LangPlugin::toConstExpInit(Loc, Type *targetType, Expression *exp)
{
    if (exp->op != TOKcall)
        return nullptr;

    auto ce = static_cast<CallExp*>(exp);
    if (ce->e1->op != TOKtype || (ce->arguments && ce->arguments->dim))
        return nullptr; // we only handle C++ structs and class values default inits

    if (targetType->ty != Tclass && targetType->ty != Tstruct)
        return nullptr;

    auto& Context = calypso.getASTContext();
    auto& CGM = calypso.CGM;

    auto DestType = Context.getRecordType(getRecordDecl(targetType)).withConst();
    return CGM->EmitNullConstant(DestType);
}

static llvm::Constant *buildAggrNullConstant(::AggregateDeclaration *decl,
        const IrAggr::VarInitMap& explicitInitializers)
{
    auto& Context = calypso.getASTContext();
    auto& CGM = calypso.CGM;

    auto RD = getRecordDecl(decl);

    if (RD->isInvalidDecl() || !RD->getDefinition())
        return nullptr;

    auto DestType = Context.getRecordType(RD).withConst();

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

    return CGM->EmitNullConstant(DestType);  // NOTE: neither EmitConstantExpr nor EmitConstantValue will work with CXXConstructExpr
}

llvm::Constant *LangPlugin::createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType)
{
    auto C = buildAggrNullConstant(irAggr->aggrdecl, explicitInitializers);  // NOTE: neither EmitConstantExpr nor EmitConstantValue will work with CXXConstructExpr

    if (!C)
        return nullptr;

    if (initializerType)
    {
        auto irSt = cast<llvm::StructType>(C->getType());
        assert(initializerType->isOpaque());
        initializerType->setBody(llvm::ArrayRef<llvm::Type*>(irSt->element_begin(),
                                irSt->element_end()),  irSt->isPacked());
    }

    // Reconstruct the constant with LDC's type, not Clang's
    // NOTE: Constant::mutateType is too dangerous because EmitNullConstant might return
    // the same constant e.g for empty records.

    if (isa<llvm::ConstantAggregateZero>(C))
        return llvm::ConstantAggregateZero::get(initializerType);
    else if (auto CS = dyn_cast<llvm::ConstantStruct>(C))
    {
        llvm::SmallVector<llvm::Constant *, 8> AggrElts;
        for (unsigned i = 0; i < initializerType->getNumElements(); i++)
            AggrElts.push_back(CS->getAggregateElement(i));
        return llvm::ConstantStruct::get(initializerType, AggrElts);
    }

    llvm_unreachable("Unhandled null constant");
}

// Required by DCXX classes, which may contain exotic fields such as class values.
void LangPlugin::addFieldInitializers(llvm::SmallVectorImpl<llvm::Constant*>& constants,
            const IrAggr::VarInitMap& explicitInitializers, ::AggregateDeclaration* decl,
            unsigned& offset, bool populateInterfacesWithVtbls)
{
    auto C = buildAggrNullConstant(decl, explicitInitializers);

    if (!C)
        return; // forward decl

    constants.push_back(C);
    offset += gDataLayout->getTypeStoreSize(C->getType());
}

LLValue* LangPlugin::toIndexAggregate(LLValue* src, ::AggregateDeclaration* ad,
                                      ::VarDeclaration* vd)
{
    auto& Context = getASTContext();
    assert(isCPP(ad) && isCPP(vd));

    auto Record = getRecordDecl(ad);
    auto Field = cast<clang::FieldDecl>(
                        static_cast<cpp::VarDeclaration*>(vd)->VD);

    updateCGFInsertPoint();

    auto LV = clang::CodeGen::LValue::MakeAddr(src, Context.getRecordType(Record),
                        clang::CharUnits::One(), Context);

    // NOTE: vd might be a field from an anon struct or union injected to ad->fields during semantic
    // LDC differs from Clang in that it also injects the fields to the LLVM type, and access to indirect fields
    // goes straight for the field instead of GEPing the anon struct first.
    // So we need to do this now:
    std::function<void(const clang::FieldDecl*)>
            Index = [&] (const clang::FieldDecl *Field)
    {
        auto Parent = Field->getParent();

        if (Parent->getCanonicalDecl() != Record->getCanonicalDecl())
        {
            const clang::FieldDecl *ParentField = nullptr;
            auto GrandParent = cast<clang::RecordDecl>(Parent->getDeclContext());

            for (auto F: GrandParent->fields())
                if (auto FRec = F->getType()->getAsCXXRecordDecl())
                    if (FRec->getCanonicalDecl() == Parent->getCanonicalDecl())
                    {
                        ParentField = F;
                        break;
                    }

            assert(ParentField);
            Index(ParentField);
        }

        LV = CGF()->EmitLValueForField(LV, Field);
    };

    Index(Field);

    if (LV.isBitField())
        return LV.getBitFieldAddr();

    return LV.getAddress();
}

void LangPlugin::toInitClass(TypeClass* tc, LLValue* dst)
{
    uint64_t const dataBytes = tc->sym->structsize;
    if (dataBytes == 0)
        return;

    LLValue* initsym = getIrAggr(tc->sym)->getInitSymbol();
    initsym = DtoBitCast(initsym, DtoClassHandleType(tc));

    DtoMemCpy(dst, initsym, DtoConstSize_t(dataBytes));
}

bool LangPlugin::toIsReturnInArg(CallExp* ce)
{
    auto FD = getFD(ce->f);
    if (isa<clang::CXXConstructorDecl>(FD) || isa<clang::CXXDestructorDecl>(FD))
        return false;

    auto& fnInfo = CGM->getTypes().arrangeFunctionDeclaration(FD);
    auto& RetAI = fnInfo.getReturnInfo();

    return RetAI.isIndirect() || RetAI.isInAlloca();
}

LLValue *LangPlugin::toVirtualFunctionPointer(DValue* inst, 
                                              ::FuncDeclaration* fdecl, char* name)
{
    updateCGFInsertPoint();
    
    auto MD = cast<clang::CXXMethodDecl>(getFD(fdecl));
    LLValue* vthis = inst->getRVal();
    auto Ty = toFunctionType(fdecl);
    
    return CGM->getCXXABI().getVirtualFunctionPointer(*CGF(), MD, vthis, Ty);
}

static const clangCG::CGFunctionInfo &arrangeFunctionCall(
                    clangCG::CodeGenModule *CGM,
                    const clang::FunctionDecl *FD,
                    clang::CodeGen::CallArgList &Args)
{
    auto FPT = FD->getType()->castAs<clang::FunctionProtoType>();
    auto MD = llvm::dyn_cast<const clang::CXXMethodDecl>(FD);

    if (MD && !MD->isStatic())
    {
        clangCG::RequiredArgs required =
            clangCG::RequiredArgs::forPrototypePlus(FPT, Args.size());

        return CGM->getTypes().arrangeCXXMethodCall(Args, FPT, required);
    }
    else
        return CGM->getTypes().arrangeFreeFunctionCall(Args, FPT, false);
}

// Emits decls with internal linkage that functions being defined or called depends upon.
class InternalDeclEmitter : public clang::RecursiveASTVisitor<InternalDeclEmitter>
{
    clang::ASTContext &Context;
    clangCG::CodeGenModule &CGM;

    llvm::DenseSet<const clang::Decl *> Emitted;

public:
    InternalDeclEmitter(clang::ASTContext &Context,
                        clangCG::CodeGenModule &CGM) : Context(Context), CGM(CGM) {}
    bool Emit(const clang::FunctionDecl *Callee);
    void Traverse(const clang::FunctionDecl *Def);

    bool VisitDeclRef(const clang::Decl *D);

    bool VisitCXXConstructExpr(const clang::CXXConstructExpr *E);
    bool VisitCXXNewExpr(const clang::CXXNewExpr *E);
    bool VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E);
    bool VisitDeclRefExpr(const clang::DeclRefExpr *E);
    bool VisitMemberExpr(const clang::MemberExpr *E);
};

void InternalDeclEmitter::Traverse(const clang::FunctionDecl *Def)
{
    TraverseStmt(Def->getBody());

    if (auto Ctor = dyn_cast<clang::CXXConstructorDecl>(Def))
        for (auto& Init: Ctor->inits())
            TraverseStmt(Init->getInit());
}

bool InternalDeclEmitter::Emit(const clang::FunctionDecl *Func)
{
    const clang::FunctionDecl *Def;

    if (!Func || !Func->hasBody(Def))
        return true;

    if (Emitted.count(Def))
        return true;
    Emitted.insert(Def);

    ResolvedFunc::get(CGM, Func);
    Traverse(Def);
    return true;
}

bool InternalDeclEmitter::VisitDeclRef(const clang::Decl *D)
{
    if (auto Func = dyn_cast<clang::FunctionDecl>(D))
        return Emit(Func);

    return true;
}

bool InternalDeclEmitter::VisitCXXConstructExpr(const clang::CXXConstructExpr *E)
{
    return Emit(E->getConstructor());
}

bool InternalDeclEmitter::VisitCXXNewExpr(const clang::CXXNewExpr *E)
{
    return Emit(E->getOperatorNew());
}

bool InternalDeclEmitter::VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E)
{
    auto DestroyedType = E->getDestroyedType();
    if (!DestroyedType.isNull()) {
        if (const clang::RecordType *RT = DestroyedType->getAs<clang::RecordType>()) {
            clang::CXXRecordDecl *RD = cast<clang::CXXRecordDecl>(RT->getDecl());
            if (RD->hasDefinition())
                Emit(RD->getDestructor());
        }
    }

    return Emit(E->getOperatorDelete());
}

bool InternalDeclEmitter::VisitDeclRefExpr(const clang::DeclRefExpr *E)
{
    return VisitDeclRef(E->getDecl());
}

bool InternalDeclEmitter::VisitMemberExpr(const clang::MemberExpr *E)
{
    return VisitDeclRef(E->getMemberDecl());
}

DValue* LangPlugin::toCallFunction(Loc& loc, Type* resulttype, DValue* fnval, 
                                   Expressions* arguments, llvm::Value *retvar)
{
    auto& Context = getASTContext();

    updateCGFInsertPoint();
    
    DFuncValue* dfnval = fnval->isFunc();
    auto fd = dfnval->func;

    // get function type info
    TypeFunction* tf = DtoTypeFunction(fnval);

    // get callee llvm value
    LLValue* callable = DtoCallableValue(fnval);
    LLFunctionType* callableTy = DtoExtractFunctionType(callable->getType());
    assert(callableTy);

    clangCG::RValue RV;
    clangCG::CallArgList Args;

    auto FD = getFD(fd);
    auto MD = dyn_cast<const clang::CXXMethodDecl>(FD);

    InternalDeclEmitter(Context, *CGM).Emit(FD);

    auto This = MD ? dfnval->vthis : nullptr;

    auto Dtor = dyn_cast<clang::CXXDestructorDecl>(FD);
    if (Dtor && Dtor->isVirtual())
    {
        CGM->getCXXABI().EmitVirtualDestructorCall(*CGF(), Dtor, clang::Dtor_Complete,
                                        This, nullptr);

        return new DImValue(nullptr, nullptr); // WARNING ldc never does that, it returns the instruction of the call site instead
    }

    // Push the this ptr.
    if (MD && !MD->isStatic())
    {
        if (MD->isVirtual()) {
            This = CGM->getCXXABI().adjustThisArgumentForVirtualFunctionCall(
                *CGF(), MD, This, true);
        }

        Args.add(clangCG::RValue::get(This),
                 MD->getThisType(getASTContext()));
    }

    size_t n = Parameter::dim(tf->parameters);
    for (size_t i=0; i<n; ++i) {
        Parameter* fnarg = Parameter::getNth(tf->parameters, i);
        assert(fnarg);
        DValue* argval = DtoArgument(fnarg, arguments->data[i]);

        auto argty = fnarg->type;
        auto ArgTy = TypeMapper().toType(loc, argty,
                                        fd->scope, fnarg->storageClass);
        if ((argty->ty == Tstruct || isClassValue(argty)) && !(fnarg->storageClass & STCref))
        {
//             llvm::Value *tmp = CGF()->CreateMemTemp(type);
//             CGF()->EmitAggregateCopy(tmp, L.getAddress(), type, /*IsVolatile*/false,
//                                 L.getAlignment());
//             Args.add(clangCG::RValue::getAggregate(tmp), type);
            Args.add(clangCG::RValue::getAggregate(argval->getRVal()),
                     ArgTy, /*NeedsCopy*/ false);
        }
        else
            Args.add(clangCG::RValue::get(argval->getRVal()), ArgTy);
    }

    clangCG::ReturnValueSlot ReturnValue(retvar, false);
    
    // Setup a landing pad if needed
    llvm::BasicBlock *invokeDest = nullptr,
                    *postinvoke = nullptr;

    auto calleeFn = dyn_cast<llvm::Function>(callable);
    auto scopes = gIR->func()->scopes;

    if (calleeFn && !calleeFn->doesNotThrow() &&
            !(scopes->cleanupScopes.empty() && scopes->catchScopes.empty()))
    {
        if (scopes->currentLandingPads().empty())
            scopes->currentLandingPads().push_back(nullptr);

        auto& landingPad = scopes->currentLandingPads().back();
        if (!landingPad)
            landingPad = scopes->emitLandingPad();

        invokeDest = landingPad;
        postinvoke = llvm::BasicBlock::Create(gIR->context(),
            "postinvoke", gIR->topfunc(), landingPad);
    }

    updateCGFInsertPoint(); // emitLandingPad() may have run the cleanups and call C++ dtors, hence changing the insert point

    auto &FInfo = arrangeFunctionCall(CGM.get(), FD, Args);
    RV = CGF()->EmitCall(FInfo, callable, ReturnValue, Args, FD,
                            nullptr, invokeDest, postinvoke);

    if (postinvoke)
        gIR->scope() = IRScope(postinvoke);

    if (isa<clang::CXXConstructorDecl>(FD))
    {
        assert(RV.isScalar() && RV.getScalarVal() == nullptr);
        return new DVarValue(resulttype, This); // EmitCall returns a null value for ctors so we need to return this
    }
    else if (tf->isref)
    {
        assert(RV.isScalar());
        return new DVarValue(resulttype, RV.getScalarVal());
    }

    if (RV.isScalar())
        return new DImValue(resulttype, RV.getScalarVal());
    else if (RV.isAggregate())
        return new DVarValue(resulttype, RV.getAggregateAddr());

    llvm_unreachable("Complex RValue FIXME");
}

void LangPlugin::toResolveFunction(::FuncDeclaration* fdecl)
{
    auto FD = getFD(fdecl);

    auto irFunc = getIrFunc(fdecl, true);
    auto &irFty = irFunc->irFty;

    fdecl->ir.setDeclared();

    if (!FD)
    {
        assert(fdecl->storage_class & STCdisable);
        return;
    }

    auto resolved = ResolvedFunc::get(*CGM, FD);
    irFunc->func = resolved.Func;
    irFty.funcType = resolved.Ty;
}

void LangPlugin::toDefineFunction(::FuncDeclaration* fdecl)
{
    auto& Context = getASTContext();

    auto FD = getFD(fdecl);
    const clang::FunctionDecl *Def;

    if (FD->hasBody(Def) && getIrFunc(fdecl)->func->isDeclaration())
    {
        CGM->EmitTopLevelDecl(const_cast<clang::FunctionDecl*>(Def)); // TODO remove const_cast

        // Emit inline functions this function depends upon
        InternalDeclEmitter(Context, *CGM).Traverse(Def);
    }
}

void LangPlugin::addBaseClassData(AggrTypeBuilder &b, ::AggregateDeclaration *base)
{
    auto RD = getRecordDecl(base);
    auto& CGRL = CGM->getTypes().getCGRecordLayout(RD);

    auto BaseTy = CGRL.getBaseSubobjectLLVMType();
    auto BaseLayout = gDataLayout->getStructLayout(BaseTy);

    for (auto Element: BaseTy->elements())
    {
        b.m_defaultTypes.push_back(Element);
        ++b.m_fieldIndex;
    }

    for (auto vd: base->fields)
    {
        auto VD = static_cast<cpp::VarDeclaration*>(vd)->VD;
        if (cast<clang::Decl>(VD->getDeclContext())->getCanonicalDecl() != RD->getCanonicalDecl())
            b.m_varGEPIndices[vd] = 0x12345678; // the field is from an anon union/struct
        else
            b.m_varGEPIndices[vd] = CGRL.getLLVMFieldNo(llvm::cast<clang::FieldDecl>(VD));
    }

    b.m_offset += BaseLayout->getSizeInBytes();
}

void LangPlugin::toDeclareVariable(::VarDeclaration* vd)
{
    auto VD = llvm::cast<clang::VarDecl>(
            static_cast<cpp::VarDeclaration*>(vd)->VD);

//     updateCGFInsertPoint();

    LLValue *V;

    // If it's thread_local, emit a call to its wrapper function instead.
//     if (VD->getTLSKind() == clang::VarDecl::TLS_Dynamic)
//         v = CGM.getCXXABI().EmitThreadLocalVarDeclLValue(*CGF, VD, VD->getType()).getAddress();
//     else
        V = CGM->GetAddrOfGlobalVar(VD);

    getIrGlobal(vd)->value = V;
}

void LangPlugin::toDefineVariable(::VarDeclaration* vd)
{
    auto& Context = getASTContext();

    auto c_vd = static_cast<cpp::VarDeclaration*>(vd);
    auto VD = cast<clang::VarDecl>(c_vd->VD);

    VD = VD->getDefinition(Context);
    if (VD && VD->hasGlobalStorage() && !VD->hasExternalStorage())
    {
        CGM->EmitTopLevelDecl(const_cast<clang::VarDecl*>(VD));

        EmittedStaticVars.push_back(VD);
        // NOTE: We need to dishonor internal linkages, which will be done after deferred decls get emitted at CGM->Release()
        // In C++ static variables are emitted for each translation unit, but since Calypso modules are more fragmented,
        // in D they need to be unique and available to other modules relying on them.
    }
}

// Handle ConstructExp initializers of struct and class vars
bool LangPlugin::toConstructVar(::VarDeclaration *vd, llvm::Value *value, Expression *rhs)
{
    // As RHS we expect either EmptyStructLiteral.this(...) for structs or null.this(...) where null is the same type as vd for classes
    // Only what's beyond the dot matters.
    // But first we must check if rhs really is such an empty literal or temporary created for the ctor call
    assert(isCPP(getAggregateSym(vd->type->toBasetype())));
    assert(rhs->op == TOKcall);

    auto ce = static_cast<CallExp*>(rhs);

    Expression *thisexp;
    if (ce->e1->op == TOKdotvar || ce->e1->op == TOKdottd)
        thisexp = static_cast<UnaExp*>(ce->e1)->e1;
    else if (ce->e1->op == TOKdotexp)
        thisexp = static_cast<BinExp*>(ce->e1)->e1;
    else
        return false;

    if (thisexp->op == TOKcomma)
    {
        thisexp = static_cast<CommaExp*>(thisexp)->e2;
        if (thisexp->op != TOKvar)
            return false;

        auto vthis = static_cast<VarExp*>(thisexp)->var;
        if (!(vthis->storage_class & STCtemp))
            return false;
    }
    else if (thisexp->op == TOKstructliteral)
    {
        return false; // TODO?
    }
    else
        return false;

    if (ce->f && !ce->f->isCtorDeclaration())
        return false; // is this enough? are we sure that A a = B(); where A and B are value types will never happen?

    DtoResolveFunction(ce->f);
    auto fnval = new DFuncValue(ce->f, getIrFunc(ce->f)->func, value);

    DtoCallFunction(ce->loc, ce->type, fnval, ce->arguments);
    return true;
}

void LangPlugin::EmitInternalDeclsForFields(const clang::RecordDecl *RD)
{
    auto& Context = getASTContext();
    auto& S = getSema();

    InternalDeclEmitter Emitter(Context, *CGM);

    auto Emit = [&] (clang::CXXMethodDecl *D) {
        if (D && !D->isDeleted())
            Emitter.Emit(D);
    };

    for (auto F: RD->fields())
    {
        auto FTyRec = F->getType()->getAsCXXRecordDecl();
        if (!FTyRec)
            continue;

        Emit(S.LookupDefaultConstructor(FTyRec));
        Emit(S.LookupDestructor(FTyRec));
    }
}

void LangPlugin::toDefineStruct(::StructDeclaration* sd)
{
    auto& S = getSema();

    if (sd->isUnionDeclaration())
        return;

    auto c_sd = static_cast<cpp::StructDeclaration*>(sd);
    auto RD = dyn_cast<clang::CXXRecordDecl>(c_sd->RD);

    if (!RD || RD->isInvalidDecl() || !RD->getDefinition())
        return;

    auto _RD = const_cast<clang::CXXRecordDecl *>(RD);
    auto Emit = [&] (clang::CXXMethodDecl *D) {
        if (D && !D->isDeleted())
        {
            ResolvedFunc::get(*CGM, D); // mark it used
            CGM->EmitTopLevelDecl(D); // mark it emittable
        }
    };

    Emit(S.LookupDefaultConstructor(_RD));
    for (int i = 0; i < 2; i++)
        Emit(S.LookupCopyingConstructor(_RD, i ? clang::Qualifiers::Const : 0));

    Emit(S.LookupDestructor(_RD));

    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 2; j++)
            for (int k = 0; k < 2; k++)
                Emit(S.LookupCopyingAssignment(_RD, i ? clang::Qualifiers::Const : 0, j ? true : false,
                                            k ? clang::Qualifiers::Const : 0));

    EmitInternalDeclsForFields(RD);
}

void LangPlugin::toDefineClass(::ClassDeclaration* cd)
{
    auto c_cd = static_cast<cpp::ClassDeclaration*>(cd);
    auto RD = cast<clang::CXXRecordDecl>(c_cd->RD);

    if (RD->isInvalidDecl() || !RD->getDefinition())
        return;

    EmitInternalDeclsForFields(RD);
}

}
