// Contributed by Elie Morisse, same license DMD uses
#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppaggregate.h"
#include "cpp/cpptemplate.h"

#include "mtype.h"
#include "target.h"
#include "gen/dvalue.h"
#include "gen/funcgenstate.h"
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
#include "clang/lib/CodeGen/ConstantEmitter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/ABI.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
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

void EmitInstantiatedDecls(std::unique_ptr<clangCG::CodeGenModule>& CGM, Dsymbol* sinst);

void LangPlugin::enterModule(::Module *m, llvm::Module *lm)
{
    auto AST = getASTUnit();
    if (!AST)
        return;

    pch.save(); // save the numerous instantiations done by DMD back into the PCH

    auto& Context = getASTContext();

    auto Opts = new clang::CodeGenOptions;
    if (global.params.symdebug)
        Opts->setDebugInfo(clang::codegenoptions::FullDebugInfo);

    CGM.reset(new clangCG::CodeGenModule(Context,
                            AST->getPreprocessor().getHeaderSearchInfo().getHeaderSearchOpts(),
                            AST->getPreprocessor().getPreprocessorOpts(),
                            *Opts, *lm, *pch.Diags));
    if (!RecordDeclTypes.empty())
        // restore the CodeGenTypes state, to prevent Clang from recreating types that end up different from the ones LDC knows
        CGM->getTypes().swapTypeCache(CGRecordLayouts, RecordDeclTypes, TypeCache);

    type_infoWrappers.clear();

    if (isCPP(m)) {
        auto c_m = static_cast<cpp::Module*>(m);
        EmitInstantiatedDecls(CGM, c_m);
        c_m->saveEmittedSymbolList();
    }
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
                appendToGlobalStors(*lm, Fn, Priority, nullptr);
            }
        }

        clangStor->eraseFromParent();
    };

    MergeGlobalStors(ldcCtor, clangCtor, "llvm.global_ctors", llvm::appendToGlobalCtors);
    MergeGlobalStors(ldcDtor, clangDtor, "llvm.global_dtors", llvm::appendToGlobalDtors);

    // HACK Check and remove duplicate module flags such as "Debug Info Version" created by both Clang and LDC
    removeDuplicateModuleFlags(lm);

    CGM->getTypes().swapTypeCache(CGRecordLayouts, RecordDeclTypes, TypeCache); // save the CodeGenTypes state
    CGM.reset();
}

void LangPlugin::enterFunc(::FuncDeclaration *fd)
{
    if (!getASTUnit())
        return;

    CGFStack.push(new clangCG::CodeGenFunction(*CGM, true));
    CGF()->CurCodeDecl = nullptr;
    CGF()->AllocaInsertPt = gIR->funcGen().allocapoint;
}

void LangPlugin::leaveFunc()
{
    if (!getASTUnit())
        return;

    CGF()->AllocaInsertPt = nullptr;
    delete CGF();
    CGFStack.pop();
}

bool LangPlugin::isEmitted(Dsymbol* s)
{
    if (!isSymbolReferenced(s))
        return false;

    if (auto fd = s->isFuncDeclaration()) {
        auto FD = getFD(fd);
        if (!FD->hasBody(FD))
            if (!FD->doesDeclarationForceExternallyVisibleDefinition())
                return false;
    }

    return true;
}

void LangPlugin::updateCGFInsertPoint()
{
    auto BB = gIR->scope().begin;
    CGF()->Builder.SetInsertPoint(BB);

    if (global.params.symdebug)
        CGF()->Builder.SetCurrentDebugLocation(
                    gIR->ir->getCurrentDebugLocation());
}

clangCG::StructorType getStructorType(clangCG::CodeGenModule &CGM, const clang::FunctionDecl *FD)
{
    clangCG::StructorType structorType = clangCG::StructorType::Complete;
    auto MD = dyn_cast<const clang::CXXMethodDecl>(FD);

    // Most of the time we only need the complete ***structor, but for abstract classes there simply isn't one
    if (MD && MD->getParent()->isAbstract())
        structorType = clangCG::StructorType::Base;

    if (isa<const clang::CXXDestructorDecl>(FD) && CGM.getTarget().getCXXABI().getKind() == clang::TargetCXXABI::Microsoft
            && MD->getParent()->getNumVBases() == 0)
        structorType = clangCG::StructorType::Base; // MSVC doesn't always emit complete dtors (aka "vbase destructor")

    return structorType;
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

        for (auto& Param: FD->parameters())
            if (isIncompleteTagType(Param->getType()))
                return result;

        auto MD = dyn_cast<const clang::CXXMethodDecl>(FD);
        clangCG::StructorType structorType = getStructorType(CGM, FD);

        // FIXME(!): in -O1+, Clang may emit internal aliases instead of dtors if a dtor matches its base class' dtor
        //  (both Itanium and MSVC)

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

        // If this is a always inlined function, emit it in any module calling or referencing it
        if (result.Func->isDeclaration() && FD->hasBody(Def) &&
                FPT->getExceptionSpecType() != clang::EST_Unevaluated)
            if (FD->hasAttr<clang::AlwaysInlineAttr>() && 
                    FD->d && FD->d->sym && calypso.isSymbolReferenced(FD->d->sym))
                CGM.EmitTopLevelDecl(const_cast<clang::FunctionDecl*>(Def));

        return result;
    }
};

void EmitRecord(std::unique_ptr<clangCG::CodeGenModule>& CGM, const clang::CXXRecordDecl* RD);

void EmitInstantiatedDecls(std::unique_ptr<clangCG::CodeGenModule>& CGM, Dsymbol* sinst)
{
    for (auto D: instantiatedDecls(sinst))
    {
        if (auto RD = dyn_cast<clang::CXXRecordDecl>(D))
            EmitRecord(CGM, RD);
        else
        {
            if (auto FD = dyn_cast<clang::FunctionDecl>(D))
            {
                ResolvedFunc::get(*CGM, FD);
                if (FD->isDefined())
                    D = FD->getDefinition();
            }

            CGM->EmitTopLevelDecl(const_cast<clang::Decl*>(D));
        }
    }
}

llvm::Type *LangPlugin::toType(::Type *t)
{
    auto& Context = getASTContext();
    auto RD = getRecordDecl(t);

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

bool LangPlugin::passAggregateArgumentByRef(AggregateDeclaration* ad)
{
    auto RD = getRecordDecl(ad);
    auto CRD = dyn_cast<clang::CXXRecordDecl>(RD);
    return CRD && CGM->getCXXABI().getRecordArgABI(CRD) == clangCG::CGCXXABI::RAA_Indirect;
}

llvm::Type *LangPlugin::IrTypeStructHijack(::StructDeclaration *sd) // HACK but if we don't do this LLVM type comparisons will fail
{
    if (sd->ident == id_cpp_member_ptr)
    {
        auto Ty = DeclMapper(nullptr, nullptr).toType(Loc(), sd->getType(), nullptr);

        if (auto MPT = Ty->getAs<clang::MemberPointerType>()) {
            auto T = CGM->getCXXABI().ConvertMemberPointerType(MPT);

            // Itanium ABI data member pointers, and MSVC ABI member pointers for POD records aren't StructType, 
            // but respectively ptrdiff_t for the earlier, and the type of the first field for the latter
            // But IrTypeStruct expects a StructType. The less complicated but imperfect solution is to return a
            // StructType here, and when passing a member pointer to Clang's CodeGen extract the first field.
            if (!isa<llvm::StructType>(T)) {
                llvm::SmallVector<llvm::Type*, 1> Elems = { T };
                T = llvm::StructType::get(gIR->context(), Elems);
            }
            return T;
        }
    }

    return nullptr;
}

llvm::Constant *LangPlugin::createStructLiteralConstant(StructLiteralExp *e)
{
    auto& Context = calypso.getASTContext();

    auto RD = getRecordDecl(e->sd);

    DeclMapper mapper(nullptr, nullptr);
    ExprMapper expmap(mapper);

    clang::APValue Value;
    expmap.toAPValue(Value, e);

    return clangCG::ConstantEmitter(*CGM).emitAbstract(clang::SourceLocation(), Value,
                                                       Context.getRecordType(RD));
}

// This is a HACK for ctor call initializers
llvm::Constant *LangPlugin::toConstElemFallback(Expression *e)
{
    auto& Context = calypso.getASTContext();

    if (e->op == TOKcall) {
        // emit and a null constant and assume that the C++ ctor gets called later
        auto ce = static_cast<CallExp*>(e);
        if (ce->e1->op != TOKtype || (ce->arguments && ce->arguments->dim))
            return nullptr; // we only handle C++ structs and class values default inits

        auto tsym = ce->e1->type->toDsymbol(nullptr);
        if (tsym->langPlugin() != this)
            return nullptr;

        if (!ce->type)
            ce->type = ce->e1->type; // Wonderful.. prevents a segfault in DtoConstExpInit
                // NOTE: e isn't semantic'd if created by get_default_initializer, which is why this is needed

        auto RD = getRecordDecl(ce->e1->type);
        auto DestType = Context.getRecordType(RD).withConst();
        return CGM->EmitNullConstant(DestType);
    }

    return nullptr;
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
    return CGM->EmitNullConstant(DestType);  // NOTE: neither EmitConstantExpr nor EmitConstantValue will work with CXXConstructExpr
}

llvm::Constant *LangPlugin::createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers)
{
    auto llStructType = irAggr->getLLStructType();

    auto C = buildAggrNullConstant(irAggr->aggrdecl, explicitInitializers);  // NOTE: neither EmitConstantExpr nor EmitConstantValue will work with CXXConstructExpr

    // Reconstruct the constant with LDC's type, not Clang's
    // NOTE: and do not use Constant::mutateType which doesn't do anything good.
    if (!C || isa<llvm::ConstantAggregateZero>(C))
        return llvm::ConstantAggregateZero::get(llStructType);
    else if (auto CS = dyn_cast<llvm::ConstantStruct>(C))
    {
        llvm::SmallVector<llvm::Constant *, 8> AggrElts;
        for (unsigned i = 0; i < llStructType->getNumElements(); i++)
            AggrElts.push_back(CS->getAggregateElement(i));
        return llvm::ConstantStruct::get(llStructType, AggrElts);
    }

    llvm_unreachable("Unhandled null constant");
}

// Required by DCXX classes, which may contain exotic fields such as class values.
bool LangPlugin::addFieldInitializers(llvm::SmallVectorImpl<llvm::Constant*>& constants,
            const IrAggr::VarInitMap& explicitInitializers, ::AggregateDeclaration* decl,
            unsigned& offset, bool populateInterfacesWithVtbls)
{
    if (decl->isStructDeclaration())
        return false; // let LDC build POD type inits

    auto C = buildAggrNullConstant(decl, explicitInitializers);

    if (!C)
        return true; // forward decl

    constants.push_back(C);
    offset += gDataLayout->getTypeStoreSize(C->getType());
    return true;
}

LLValue* LangPlugin::toIndexAggregate(LLValue* src, ::AggregateDeclaration* ad,
                                      ::VarDeclaration* vd)
{
    auto& Context = getASTContext();
    assert(isCPP(vd));

    auto parent = vd->toParent2()->isAggregateDeclaration();
    assert(parent && isCPP(parent));

    auto srcType = ad->getType();
    if (!ad->isClassDeclaration() || isCPP(ad))
        srcType = srcType->pointerTo();

    if (ad != parent) {
        Loc loc;
        DImValue srcValue(srcType, src);

        src = DtoRVal(DtoCast(loc, &srcValue, parent->getType()->pointerTo()));
    }

    auto Record = getRecordDecl(parent);
    auto Field = cast<clang::FieldDecl>(
                        static_cast<cpp::VarDeclaration*>(vd)->VD);

    updateCGFInsertPoint();

    clangCG::Address address(src, clang::CharUnits::One());
    auto LV = CGF()->MakeAddrLValue(address, Context.getRecordType(Record),
                                    clangCG::AlignmentSource::Type);

    // NOTE: vd might be a field from an anon struct or union injected to parent->fields during semantic
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
        return LV.getBitFieldPointer();

    return LV.getPointer();
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

DValue *LangPlugin::toDynamicCast(Loc &loc, DValue *val, Type *_to)
{
    DeclMapper mapper(nullptr, nullptr);

    auto SrcRecordTy = mapper.toType(loc, getAggregateHandle(val->type)->getType(), nullptr);
    auto DestTy = mapper.toType(loc, _to, nullptr);
    auto DestRecordTy = mapper.toType(loc, getAggregateHandle(_to)->getType(), nullptr);
    clangCG::Address This(DtoRVal(val), clang::CharUnits::One());

    assert(!DestTy->isReferenceType()); // makes sure that the CastEnd argument won't be used

    updateCGFInsertPoint();
    auto ret = CGM->getCXXABI().EmitDynamicCastCall(
                            *CGF(), This, SrcRecordTy, DestTy, DestRecordTy, nullptr);

    return new DImValue(_to, ret);
}

DValue *LangPlugin::adjustForDynamicCast(Loc &loc, DValue *val, Type *_to)
{
    TypeClass *to = static_cast<TypeClass *>(_to->toBasetype());
    auto adfrom = getAggregateHandle(val->type);
    auto cdto = to->sym;
    assert(!cdto->langPlugin());

    // WARNING: this assumes that val is a pointer towards a C++ part of a DCXX class
    int offset;
    bool isUpcastable = adfrom->isBaseOf(cdto, &offset);
    assert(isUpcastable);

    LLValue *v = DtoClassHandle(val);
    LLType* tolltype = DtoAggregateHandleType(to);
    LLValue* rval = DtoBitCast(v,
            llvm::Type::getInt8PtrTy(gIR->context()));
    if (offset) {
        auto baseOffset = llvm::ConstantInt::get(
                    DtoType(Type::tptrdiff_t), -offset);
        rval = gIR->ir->CreateGEP(rval,
                                        baseOffset, "sub.ptr");
    }
    rval = DtoBitCast(rval, tolltype);
    return DtoAggregateDValue(to, rval);
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
                                              ::FuncDeclaration* fdecl, const char* name)
{
    updateCGFInsertPoint();
    
    auto MD = cast<clang::CXXMethodDecl>(getFD(fdecl));
    LLValue* vthis = DtoLVal(inst);
    auto Ty = toFunctionType(fdecl);
    
    clangCG::Address This(vthis, clang::CharUnits::One());
    auto F = CGM->getCXXABI().getVirtualFunctionPointer(
                            *CGF(), MD, This, Ty, clang::SourceLocation());
    return F.getFunctionPointer();
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
            clangCG::RequiredArgs::forPrototypePlus(FPT, Args.size(), FD);

        return CGM->getTypes().arrangeCXXMethodCall(Args, FPT, required, /*numPrefixArgs=*/0); // NOTE: numPrefixArgs is always 0 except during dtor calls that need a VTT
    }
    else
        return CGM->getTypes().arrangeFreeFunctionCall(Args, FPT, false);
}

DValue* LangPlugin::toCallFunction(Loc& loc, Type* resulttype, DValue* fnval, 
                                   Expressions *arguments, llvm::Value *retvar)
{
    updateCGFInsertPoint();
    
    DFuncValue* dfnval = fnval->isFunc();
    auto fd = dfnval->func;

    // get function type info
    TypeFunction* tf = DtoTypeFunction(fnval);

    // get callee llvm value
    LLValue* callable = DtoCallableValue(fnval);

    clangCG::RValue RV;
    clangCG::CallArgList Args;

    auto FD = getFD(fd);
    auto MD = dyn_cast<const clang::CXXMethodDecl>(FD);

    auto ThisVal = MD ? dfnval->vthis : nullptr;
    clangCG::Address This(ThisVal, clang::CharUnits::One());

    auto Dtor = dyn_cast<clang::CXXDestructorDecl>(FD);
    if (Dtor && Dtor->isVirtual())
    {
        CGM->getCXXABI().EmitVirtualDestructorCall(*CGF(), Dtor, clang::Dtor_Complete,
                                        This, nullptr);

        // EmitVirtualDestructorCall doesn't return the call site instruction, and a non-null return value is
        // expected but unused, so anything should work
        return new DImValue(Type::tvoidptr,
                            llvm::Constant::getNullValue(DtoType(Type::tvoidptr)));
    }

    // Push the this ptr.
    if (MD && !MD->isStatic())
    {
        if (MD->isVirtual()) {
            This = CGM->getCXXABI().adjustThisArgumentForVirtualFunctionCall(
                *CGF(), MD, This, true);
        }

        Args.add(clangCG::RValue::get(This.getPointer()),
                 MD->getThisType(getASTContext()));
    }

    size_t n = Parameter::dim(tf->parameters);
    for (size_t i = 0; i < n; ++i) {
        Parameter* fnarg = Parameter::getNth(tf->parameters, i);
        assert(fnarg);
        DValue* argval = toElem((*arguments)[i]);

        auto argty = fnarg->type;
        auto ArgTy = DeclMapper(nullptr, nullptr).toType(loc, argty,
                                        fd->_scope, fnarg->storageClass);

        auto ad = getAggregateSym(argval->type);

        llvm::Value* val;
        if ((fnarg->storageClass & STCref) && argval->isRVal())
        {
            auto PointeeTy = cast<clang::ReferenceType>(ArgTy)->getPointeeType();
            auto Tmp = CGF()->CreateMemTemp(PointeeTy, "ref.tmp");
            clangCG::LValue RefTempDst = CGF()->MakeAddrLValue(Tmp, PointeeTy,
                                                    clangCG::AlignmentSource::Type);

            auto RVal = clangCG::RValue::get(DtoRVal(argval));
            CGF()->EmitStoreThroughLValue(RVal, RefTempDst, true);

            val = RefTempDst.getPointer();
            // IMPORTANT FIXME RefTempDst needs to be cleaned up, and this should appear within the AST
            //  with something akin to MaterializeTemporaryExpr
        }
        else
            val = (fnarg->storageClass & STCref || DtoIsInMemoryOnly(argval->type))
                    ? DtoLVal(argval) : DtoRVal(argval);

        if (isa<clang::MemberPointerType>(ArgTy) && ad->fields.dim == 1)
        {
            // Special case for "single field" member pointers.. not ideal, but had to compromise
            val = DtoIndexAggregate(val, ad, ad->fields[0]);
            val = DtoLoad(val, "extractmptrfield");

            Args.add(clangCG::RValue::get(val), ArgTy);
        }
        else if ((argty->ty == Tstruct || isClassValue(argty)) && !(fnarg->storageClass & STCref))
        {
//             llvm::Value *tmp = CGF()->CreateMemTemp(type);
//             CGF()->EmitAggregateCopy(tmp, L.getAddress(), type, /*IsVolatile*/false,
//                                 L.getAlignment());
//             Args.add(clangCG::RValue::getAggregate(tmp), type);
            clangCG::Address addr(val, CGF()->getNaturalTypeAlignment(ArgTy));
            Args.add(clangCG::RValue::getAggregate(addr), ArgTy);
        }
        else
            Args.add(clangCG::RValue::get(val), ArgTy);
    }

    clangCG::Address Addr(retvar,
                          CGF()->getNaturalTypeAlignment(FD->getReturnType()));
    clangCG::ReturnValueSlot ReturnValue(Addr, false);
    
    // Setup a landing pad if needed
    llvm::BasicBlock *invokeDest = nullptr,
                    *postinvoke = nullptr;

    auto calleeFn = dyn_cast<llvm::Function>(callable);
    auto& scopes = gIR->funcGen().scopes;

    if (calleeFn && !calleeFn->doesNotThrow() && !scopes.empty())
    {
        invokeDest = scopes.getLandingPad();
        postinvoke = llvm::BasicBlock::Create(gIR->context(),
            "postinvoke", gIR->topfunc(), invokeDest);
    }

    updateCGFInsertPoint(); // emitLandingPad() may have run the cleanups and call C++ dtors, hence changing the insert point

    auto &FInfo = arrangeFunctionCall(CGM.get(), FD, Args);
    llvm::Instruction *callOrInvoke;
    RV = CGF()->EmitCall(FInfo, clangCG::CGCallee(FD, callable), ReturnValue, Args,
                            &callOrInvoke, invokeDest, postinvoke);

    if (postinvoke)
        gIR->scope() = IRScope(postinvoke);

    if (isa<clang::CXXConstructorDecl>(FD))
    {
        assert(RV.isScalar() && RV.getScalarVal() == nullptr);
        auto calleead = fd->isMember2();
        auto thisTy = calleead->getType();
        if (!calleead->byRef())
            thisTy = thisTy->pointerTo();  // resulttype isn't always the This type

        // Mature the C++ vptrs set by C++ constructors after a super() call
        if (resulttype->ty == Tclass)
        {
            auto tc = static_cast<TypeClass*>(resulttype);
            if (calleead->isBaseOf(tc->sym, nullptr))
                toPostNewClass(loc, tc, new DImValue(thisTy, This.getPointer()));
        }

        return new DLValue(calleead->getType(), This.getPointer()); // EmitCall returns a null value for ctors so we need to return this
    }
    else if (tf->isref)
    {
        assert(RV.isScalar());
        return new DLValue(resulttype, RV.getScalarVal());
    }

    if (resulttype->ty == Tvoid)
        return new DImValue(resulttype, callOrInvoke);
    else if (RV.isScalar())
        return new DImValue(resulttype, RV.getScalarVal());
    else if (RV.isAggregate())
        return new DLValue(resulttype, RV.getAggregatePointer());

    llvm_unreachable("Complex RValue FIXME");
}

void LangPlugin::toResolveFunction(::FuncDeclaration* fdecl)
{
    auto FD = getFD(fdecl);

    auto irFunc = getIrFunc(fdecl, true);
    auto &irFty = irFunc->irFty;

    fdecl->ir->setDeclared();

    if (!FD)
    {
        assert(fdecl->storage_class & STCdisable);
        return;
    }

    auto resolved = ResolvedFunc::get(*CGM, FD);
    if (!resolved.Func)
    {
        assert(!getIsUsed(fdecl));
        return;
    }
    irFunc->setLLVMFunc(resolved.Func);
    irFty.funcType = resolved.Ty;
}

void LangPlugin::toDefineFunction(::FuncDeclaration* fdecl)
{
    if (!getIsUsed(fdecl))
        return;

    auto FD = getFD(fdecl);
    const clang::FunctionDecl *Def;

    if (FD->hasBody(Def) && !Def->isInvalidDecl() && getIrFunc(fdecl)->getLLVMFunc()->isDeclaration())
        CGM->EmitTopLevelDecl(const_cast<clang::FunctionDecl*>(Def)); // TODO remove const_cast

    EmitInstantiatedDecls(CGM, fdecl);
}

void LangPlugin::addBaseClassData(AggrTypeBuilder &b, ::AggregateDeclaration *base)
{
    auto RD = getRecordDecl(base);
    auto& CGRL = CGM->getTypes().getCGRecordLayout(RD);

    auto BaseTy = CGRL.getLLVMType();
    auto BaseLayout = gDataLayout->getStructLayout(BaseTy);

    b.m_defaultTypes.push_back(BaseTy);
    ++b.m_fieldIndex;

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
        CGM->EmitTopLevelDecl(const_cast<clang::VarDecl*>(VD));
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
    else if (ce->e1->op == TOKdot)
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
    auto fnval = new DFuncValue(ce->f, getIrFunc(ce->f)->getLLVMFunc(), value);

    DtoCallFunction(ce->loc, ce->type, fnval, ce->arguments);
    return true;
}

// // Even if never visible from D, C++ functions may depend on these methods, so they still need to be emitted
// static void EmitUnmappedRecordMethods(clangCG::CodeGenModule& CGM,
//                                       clang::Sema& S,
//                                       clang::CXXRecordDecl* RD)
// {
//     if (!RD || RD->isInvalidDecl() || !RD->getDefinition())
//         return;
//
//     auto Emit = [&] (clang::CXXMethodDecl *D) {
//         if (!D || D->isInvalidDecl() || D->isDeleted())
//             return;
//
//         if (!isMapped(D))
//         {
//             auto R = ResolvedFunc::get(CGM, D); // mark it used
//             if (R.Func->isDeclaration())
//                 CGM.EmitTopLevelDecl(D); // mark it emittable
//         }
//
//         // For MSVC
//         if (CGM.getTarget().getCXXABI().getKind() == clang::TargetCXXABI::Microsoft) {
//             if (auto Dtor = dyn_cast<const clang::CXXDestructorDecl>(D)) {
//                 auto structorType = Dtor->getParent()->getNumVBases() == 0 ?
//                     clangCG::StructorType::Complete : clangCG::StructorType::Base;
//                 auto FInfo = &CGM.getTypes().arrangeCXXStructorDeclaration(Dtor, structorType);
//                 CGM.getAddrOfCXXStructor(Dtor, structorType, FInfo,
//                     CGM.getTypes().GetFunctionType(*FInfo), true);
//                 CGM.EmitGlobal(clang::GlobalDecl(Dtor, clangCG::toCXXDtorType(structorType)));
//             }
//         }
//     };
//
//     Emit(S.LookupDefaultConstructor(RD));
//     for (int i = 0; i < 2; i++)
//         Emit(S.LookupCopyingConstructor(RD, i ? clang::Qualifiers::Const : 0));
//
//     Emit(S.LookupDestructor(RD));
//
//     for (int i = 0; i < 2; i++)
//         for (int j = 0; j < 2; j++)
//             for (int k = 0; k < 2; k++)
//                 Emit(S.LookupCopyingAssignment(RD, i ? clang::Qualifiers::Const : 0, j ? true : false,
//                                             k ? clang::Qualifiers::Const : 0));
// }

void LangPlugin::toDefineStruct(::StructDeclaration* decl)
{
    auto c_sd = static_cast<cpp::StructDeclaration*>(decl);
    if (c_sd->RD->isInvalidDecl() || decl->isUnionDeclaration())
        return;

//     if (c_sd->isUsed) {
//         auto& S = getSema();
//         if (auto RD = dyn_cast<clang::CXXRecordDecl>(c_sd->RD))
//             EmitUnmappedRecordMethods(*CGM, S,
//                 const_cast<clang::CXXRecordDecl *>(RD));
//     }

    // Define the __initZ symbol.
    IrAggr *ir = getIrAggr(decl);
    auto &initZ = ir->getInitSymbol();
    auto initGlobal = llvm::cast<LLGlobalVariable>(initZ);
    assert(initGlobal);
    setLinkage(decl, initGlobal);
    initZ = gIR->setGlobalVarInitializer(initGlobal, ir->getDefaultInit());

    // emit typeinfo
    DtoTypeInfoOf(decl->type);

    if (c_sd->isUsed) {
        // Emit __xopEquals/__xopCmp/__xtoHash.
        if (decl->xeq && decl->xeq != decl->xerreq) {
          Declaration_codegen(decl->xeq);
        }
        if (decl->xcmp && decl->xcmp != decl->xerrcmp) {
          Declaration_codegen(decl->xcmp);
        }
        if (decl->xhash) {
          Declaration_codegen(decl->xhash);
        }
    }
}

void EmitRecord(std::unique_ptr<clangCG::CodeGenModule>& CGM, const clang::CXXRecordDecl* RD)
{
    auto& Context = calypso.getASTContext();
//     auto& S = getSema();

    const clang::FunctionDecl *keyDef = nullptr;
    const clang::CXXMethodDecl *key =
                Context.getCurrentKeyFunction(RD);
    if (key && !RD->hasAttr<clang::DLLImportAttr>())
        key->hasBody(keyDef);

    CGM->RecordBeingDefined = RD;
    if (RD->isDynamicClass() && (!key || keyDef))
        CGM->EmitVTable(const_cast<clang::CXXRecordDecl*>(RD));

//     EmitUnmappedRecordMethods(*CGM, S,
//         const_cast<clang::CXXRecordDecl *>(c_cd->RD));

    CGM->RecordBeingDefined = nullptr;
}

void LangPlugin::toDefineClass(::ClassDeclaration* decl)
{
    auto c_cd = static_cast<cpp::ClassDeclaration*>(decl);
    if (c_cd->RD->isInvalidDecl())
        return;

    if (c_cd->isUsed)
        EmitRecord(CGM, c_cd->RD);

    IrAggr *ir = getIrAggr(decl);
    const auto lwc = DtoLinkage(decl);

    auto &initZ = ir->getInitSymbol();
    auto initGlobal = llvm::cast<LLGlobalVariable>(initZ);
    assert(initGlobal);
    setLinkage(lwc, initGlobal);
    initZ = gIR->setGlobalVarInitializer(initGlobal, ir->getDefaultInit());

//     llvm::GlobalVariable *vtbl = ir->getVtblSymbol();
//     vtbl->setInitializer(ir->getVtblInit());
//     setLinkage(lwc, vtbl);

    llvm::GlobalVariable *classZ = ir->getClassInfoSymbol();
    classZ->setInitializer(ir->getClassInfoInit());
    setLinkage(lwc, classZ);
}

}
