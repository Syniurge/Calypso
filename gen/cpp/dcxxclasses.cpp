// Contributed by Elie Morisse, same license DMD uses
#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppaggregate.h"
#include "cpp/ddmdstructor.h"

#include "identifier.h"
#include "init.h"
#include "target.h"
#include "ir/irfunction.h"
#include "gen/irstate.h"
#include "gen/llvmhelpers.h"

#include "clang/CodeGen/ConstantInitBuilder.h"
#include "clang/lib/CodeGen/CodeGenFunction.h"
#include "clang/lib/CodeGen/CodeGenTypes.h"
#include "clang/lib/CodeGen/CGCXXABI.h"

//////////////////////////////////////////////////////////////////////////////////////////

namespace clang {
    namespace CodeGen {
        llvm::GlobalVariable *getMSCompleteObjectLocator(CGCXXABI& CXXABI, const CXXRecordDecl *RD,
                                                         const VPtrInfo &Info);
    }
}

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

namespace clangCG = clang::CodeGen;

// A D class derived from a C++ one overriding virtual C++ methods must,
// after the last C++ ctor call, make the C++ vptrs point to a modified vtable.
// The trick is to make Clang recreate a VTable initializer for the most derived C++ base
// and then hack on it by replacing the thunk addresses with new ones.

struct DCXXVTableInfo // "DCXX" for D-C++ class "hybrid"
{
    cpp::ClassDeclaration *mostDerivedCXXBase;
    const clang::CXXRecordDecl* MostDerivedBase;

    // Only relevant for the Itanium ABI
    const clang::VTableLayout *VTLayout;
    llvm::StructType *VTStructType;

    static DCXXVTableInfo *get(::ClassDeclaration *cd)
    {
        auto result = new DCXXVTableInfo;

        auto& Context = calypso.getASTContext();

        auto cd2 = isDCXX(cd);
        if (!cd2)
            return result;  // Pure C++ or D class

        auto& CGM = *calypso.CGM;
        auto& CGVT = CGM.getVTables();
        auto& VTableContext =
            *static_cast<clang::ItaniumVTableContext *>(Context.getVTableContext());

        result->mostDerivedCXXBase = static_cast<cpp::ClassDeclaration *>(cd2);
        result->MostDerivedBase = result->mostDerivedCXXBase->RD;

        if (!Context.getVTableContext()->isMicrosoft()) {
            result->VTLayout =
                &VTableContext.getVTableLayout(result->MostDerivedBase);
            result->VTStructType = cast<llvm::StructType>(
                                        CGVT.getVTableType(*result->VTLayout));
        }

        return result;
    }
};

static inline llvm::GlobalVariable *getDCXXVTable(::ClassDeclaration *cd,
                                           DCXXVTableInfo *dcxxInfo = nullptr,
                                           clang::CharUnits BaseOffset = clang::CharUnits::Zero(), // non-zero if Microsoft ABI
                                           const clang::VTableLayout* VTLayout = nullptr)
{
    assert(isDCXX(cd));

    auto& CGM = *calypso.CGM;
    auto& CGVT = CGM.getVTables();

    if (!dcxxInfo)
        dcxxInfo = DCXXVTableInfo::get(cd);

    auto VTStructType = dcxxInfo->VTStructType;
    if (VTLayout)
        VTStructType = cast<llvm::StructType>(
                            CGVT.getVTableType(*VTLayout));

    OutBuffer mangledName;
    mangleToBuffer(cd, &mangledName);

    std::string initname("_D");
    initname.append(mangledName.peekString());
    initname.append("9__VtblCXXZ");
    if (!BaseOffset.isZero())
        initname.append(std::to_string(BaseOffset.getQuantity()));

    auto dcxxVTable = getOrCreateGlobal(cd->loc,
        gIR->module, VTStructType, false,
        llvm::GlobalValue::ExternalLinkage, NULL, initname);

    return dcxxVTable;
}

static void mangleNumber(llvm::raw_ostream &Out,
                         int64_t Number)
{
  //  <number> ::= [n] <non-negative decimal integer>
  if (Number < 0) {
    Out << 'n';
    Number = -Number;
  }

  Out << Number;
}

static clang::BaseOffset
ComputeReturnAdjustmentBaseOffset(clang::ASTContext &Context,
                                  const ::FuncDeclaration *overmd,
                                  const clang::CXXMethodDecl *BaseMD) {
    return clang::BaseOffset(); // FIXME
//   const clang::FunctionType *BaseFT = BaseMD->getType()->getAs<clang::FunctionType>();
//   const FunctionType *DerivedFT = DerivedMD->getType()->getAs<FunctionType>();
//
//   // Canonicalize the return types.
//   clang::CanQualType CanDerivedReturnType =
//       Context.getCanonicalType(DerivedFT->getReturnType());
//   clang::CanQualType CanBaseReturnType =
//       Context.getCanonicalType(BaseFT->getReturnType());
//
//   assert(CanDerivedReturnType->getTypeClass() ==
//          CanBaseReturnType->getTypeClass() &&
//          "Types must have same type class!");
//
//   if (CanDerivedReturnType == CanBaseReturnType) {
//     // No adjustment needed.
//     return clang::BaseOffset();
//   }
//
//   if (isa<clang::ReferenceType>(CanDerivedReturnType)) {
//     CanDerivedReturnType =
//       CanDerivedReturnType->getAs<ReferenceType>()->getPointeeType();
//     CanBaseReturnType =
//       CanBaseReturnType->getAs<ReferenceType>()->getPointeeType();
//   } else if (isa<clang::PointerType>(CanDerivedReturnType)) {
//     CanDerivedReturnType =
//       CanDerivedReturnType->getAs<PointerType>()->getPointeeType();
//     CanBaseReturnType =
//       CanBaseReturnType->getAs<PointerType>()->getPointeeType();
//   } else {
//     llvm_unreachable("Unexpected return type!");
//   }
//
//   // We need to compare unqualified types here; consider
//   //   const T *Base::foo();
//   //   T *Derived::foo();
//   if (CanDerivedReturnType.getUnqualifiedType() ==
//       CanBaseReturnType.getUnqualifiedType()) {
//     // No adjustment needed.
//     return clang::BaseOffset();
//   }
//
//   const clang::CXXRecordDecl *DerivedRD =
//     cast<clang::CXXRecordDecl>(cast<clang::RecordType>(CanDerivedReturnType)->getDecl());
//
//   const clang::CXXRecordDecl *BaseRD =
//     cast<clang::CXXRecordDecl>(cast<clang::RecordType>(CanBaseReturnType)->getDecl());
//
//   return clang::ItaniumVTableBuilder::ComputeBaseOffset(Context, BaseRD, DerivedRD);
}

// HACK? We're generating C++ thunk-like functions at codegen time!
// Since they are final they shouldn't affect the aggregate they're member of, but still...
//
// Pro: this avoids redundant "manual" function code generation (even though thunks are simple)
// Con: Clang's way of doing things avoids meddling with the AST, doesn't have any hard-to-foresee consequence
::FuncDeclaration *getDCXXThunk(::FuncDeclaration *callee,
                             const clang::ThunkInfo &Thunk)
{
    assert(!isCPP(callee));

    auto loc = callee->loc;

    // generate a name
    llvm::SmallString<256> thunkName;
    llvm::raw_svector_ostream Out(thunkName);

    Out << "_DCXT";
    mangleNumber(Out, Thunk.This.NonVirtual);
    Out << '_';
    Out << callee->ident->toChars();

    auto thunkId = Identifier::idPool(thunkName.c_str(), thunkName.size());
    auto calleetf = static_cast<TypeFunction*>(callee->type);

    // check if the thunk already exists
    auto parent = static_cast<::ClassDeclaration*>(callee->isThis());
    if (auto fd = parent->findFunc(thunkId, calleetf))
        return fd;

    Type *tf = new_TypeFunction(calleetf->parameters,
                                calleetf->next, 0, LINKcpp, STCfinal);
    auto fthunk = new_FuncDeclaration(loc, loc,
                    thunkId, STCfinal, tf);

    // build arg list
    auto params = calleetf->parameters;
    auto args = new Expressions;
    args->reserve(params->dim);
    for (auto *p: *params)
        args->push(new_IdentifierExp(loc, p->ident));

    // adjust "this"
    auto idtmp = Identifier::generateId("__tmp");
    auto tmp = new_VarDeclaration(loc, Type::tvoidptr,
                             idtmp, new_VoidInitializer(loc));
    tmp->storage_class |= STCtemp | STCctfe;
    Expression *e = new_DeclarationExp(loc, tmp);
    Expression *ec = new_BlitExp(loc, new_VarExp(loc, tmp), new_ThisExp(loc));
    e = Expression::combine(e, ec);
    ec = new_AddAssignExp(loc, new_VarExp(loc, tmp),
                          new_IntegerExp(Thunk.This.NonVirtual));
    e = Expression::combine(e, ec);
    Statement *s1 = new_ExpStatement(loc, e);

    // emit call
    ec = new_CastExp(loc, new_VarExp(loc, tmp), parent->type);
    ec = new_DotVarExp(loc, ec, callee);
    ec = new_CallExp(loc, ec, args);

    // TODO adjust "this" back if returned

    Statement *s2 = new_ReturnStatement(loc, ec);
    fthunk->fbody = new_CompoundStatement(loc, s1, s2);

    fthunk->importAll(callee->_scope);
    semantic(fthunk, callee->_scope);
    fthunk->protection = {PROTprivate, nullptr};  // HACK NOTE: setting the prot to private will bypass the invariant checks
    semantic2(fthunk, callee->_scope);
    semantic3(fthunk, callee->_scope);
    //fprintf(stderr, "%s", fthunk->fbody->toChars());
    Declaration_codegen(fthunk);

    return fthunk;
}

template<class BuilderTy> struct VTableBuilder {};
template<> struct VTableBuilder<clang::ItaniumVTableBuilder> {
    typedef clang::ItaniumVTableContext VTableContextTy;

    clang::ItaniumVTableBuilder B;
    const clang::CXXRecordDecl *MostDerivedClass;

    VTableBuilder(clang::VTableContextBase *VTables,
        const clang::CXXRecordDecl *MostDerivedClass,
        const clang::VPtrInfo* = nullptr)
        : B(*static_cast<clang::ItaniumVTableContext *>(VTables),
            MostDerivedClass, clang::CharUnits::Zero(),
            /*MostDerivedClassIsVirtual=*/false, MostDerivedClass),
          MostDerivedClass(MostDerivedClass) {}

    inline clang::CharUnits ComputeThisAdjustment(
        const clang::CXXMethodDecl* MD)
    {
        auto& Context = MD->getASTContext();

        clang::BaseSubobject OverriddenBaseSubobject(MD->getParent(),
            clang::ComputeBaseOffset_(Context, MD->getParent(), MostDerivedClass).NonVirtualOffset);
        clang::BaseSubobject OverriderBaseSubobject(MostDerivedClass, clang::CharUnits::Zero());

        clang::BaseOffset ThisOffset = B.ComputeThisAdjustmentBaseOffset(OverriddenBaseSubobject,
            OverriderBaseSubobject);
        return ThisOffset.NonVirtualOffset;
    }

    inline clang::ReturnAdjustment ComputeReturnAdjustment(
        clang::BaseOffset Offset) {
        return B.ComputeReturnAdjustment(Offset);
    }
};
template<> struct VTableBuilder<clang::VFTableBuilder> {
    typedef clang::MicrosoftVTableContext VTableContextTy;

    clang::VFTableBuilder B;

    VTableBuilder(clang::VTableContextBase *VTables,
        const clang::CXXRecordDecl *MostDerivedClass,
        const clang::VPtrInfo* Which)
        : B(*static_cast<clang::MicrosoftVTableContext *>(VTables),
            MostDerivedClass, *Which) {}

    inline clang::CharUnits ComputeThisAdjustment(
        const clang::CXXMethodDecl* MD) {
        auto& Context = MD->getASTContext();

        clang::BaseSubobject OverriddenBaseSubobject(MD->getParent(),
            clang::ComputeBaseOffset_(Context, MD->getParent(), B.MostDerivedClass).NonVirtualOffset);
        //clang::BaseSubobject OverriderBaseSubobject(MostDerivedClass, clang::CharUnits::Zero());

        clang::FinalOverriders::OverriderInfo FinalOverrider =
            B.Overriders.getOverrider(MD, OverriddenBaseSubobject.getBaseOffset());
        return -B.ComputeThisOffset(FinalOverrider);
    }

    inline clang::ReturnAdjustment ComputeReturnAdjustment(
        clang::BaseOffset Offset) {
        return clang::ReturnAdjustment(); // FIXME
    }
};

template <class BuilderTy>
struct DCXXVTableAdjuster
{
    ::ClassDeclaration *cd;
    DCXXVTableInfo& dcxxInfo;
    clangCG::CodeGenModule& CGM;
    clang::ASTContext& Context;

    DCXXVTableAdjuster(::ClassDeclaration *cd, DCXXVTableInfo& dcxxInfo, 
            clangCG::CodeGenModule& CGM, clang::ASTContext& Context)
        : cd(cd), dcxxInfo(dcxxInfo), CGM(CGM), Context(Context) {}

    void adjustVTableInitializer(llvm::GlobalVariable *VTable,
                const clang::VTableLayout &VTLayout,
                llvm::Constant* RTTI, clang::VPtrInfo *Info = nullptr)
    {
        auto& CGVT = CGM.getVTables();

        VTableBuilder<BuilderTy> Builder(Context.getVTableContext(), 
                    dcxxInfo.MostDerivedBase, Info);

        clangCG::ConstantInitBuilder builder(CGM);
        auto components = builder.beginStruct();
        CGVT.createVTableInitializer(components, VTLayout, RTTI);
        components.finishAndSetAsInitializer(VTable);

        auto OldInit = VTable->getInitializer();

        // Copy the operands into a new array
        // NOTE: replaceUsesOfWithOnConstant isn't suitable because it'll replace
        // the C++ class vtable by the DCXX one.
        std::vector<std::vector<llvm::Constant*>> Init(
                    OldInit->getNumOperands());

        for (unsigned i = 0; i < OldInit->getNumOperands(); i++) {
            auto* Op = cast<llvm::Constant>(OldInit->getOperand(i));
            Init[i].reserve(Op->getNumOperands());
            for (unsigned j = 0; j < Op->getNumOperands(); j++)
                Init[i].push_back(llvm::cast<llvm::Constant>(Op->getOperand(j)));
        }

        // search for virtual C++ methods overriden by the D class
        for (auto s : cd->vtbl)
        {
            auto md = s->isFuncDeclaration();
            if (!md)
                continue;

            cpp::FuncDeclaration *cxxmd = nullptr;
            if (auto overmd = findOverriddenMethod(md, dcxxInfo.mostDerivedCXXBase))
            {
                assert(isCPP(overmd));
                cxxmd = static_cast<cpp::FuncDeclaration*>(overmd);
            }

            if (!cxxmd)
                continue;   // md isn't overriding a C++ method

            auto MD = llvm::cast<clang::CXXMethodDecl>(
                getFD(cxxmd)->getCanonicalDecl());

            // Replace calls to MD with calls to md properly adjusted by thunks
            for (unsigned I = 0, E = VTLayout.getNumVTables(); I != E; ++I) {
                auto& InitElem = Init[I];
                size_t thisIndex = VTLayout.getVTableOffset(I);
                size_t nextIndex = thisIndex + VTLayout.getVTableSize(I);

                for (unsigned I = thisIndex; I != nextIndex; ++I) {
                    auto& Component = VTLayout.vtable_components()[I];
                    const clang::CXXMethodDecl *CompMD;;

                    switch (Component.getKind())
                    {
                    case clang::VTableComponent::CK_FunctionPointer:
                        CompMD = Component.getFunctionDecl()->getCanonicalDecl();
                        break;
                    case clang::VTableComponent::CK_CompleteDtorPointer:
                        CompMD = Component.getDestructorDecl()->getCanonicalDecl();
                        break;
                    default:
                        CompMD = nullptr;
                    }

                    if (!CompMD || CompMD != MD)
                        continue;

                    clang::ThunkInfo NewThunk;
                    NewThunk.This.NonVirtual = -2 * Target::ptrsize;
                    NewThunk.Return.NonVirtual = 2 * Target::ptrsize;

                    if (MD->getParent()->getCanonicalDecl() != dcxxInfo.MostDerivedBase->getCanonicalDecl())
                    {
                        // NOTE: we can't rely on existing thunks, because methods that aren't overridden by the most derived C++ class
                        // won't have the proper thunk offsets (sometimes no thunk at all).

                        // This adjustment.
                        auto ThisOffset = Builder.ComputeThisAdjustment(MD);
                        NewThunk.This.NonVirtual += ThisOffset.getQuantity();

                        // Return adjustment.
                        clang::BaseOffset ReturnAdjustmentOffset;
                        ReturnAdjustmentOffset = ComputeReturnAdjustmentBaseOffset(Context, md, MD);
                        NewThunk.Return.NonVirtual += Builder.ComputeReturnAdjustment(ReturnAdjustmentOffset).NonVirtual;

    //                 NewThunk.This.Virtual.Itanium.VCallOffsetOffset = ; TODO
                    }

                    auto thunkFd = getDCXXThunk(md, NewThunk);
                    auto thunkLLFunc = getIrFunc(thunkFd)->getLLVMFunc();
                    InitElem[I] = llvm::ConstantExpr::getBitCast(thunkLLFunc, CGM.Int8PtrTy);
                }
            }
        }

        std::vector<llvm::Constant*> NewInitElems;
        size_t i = 0;
        for (auto& InitElem: Init)
            NewInitElems.push_back(llvm::ConstantArray::get(
                    cast<llvm::ArrayType>(OldInit->getOperand(i++)->getType()), InitElem));

        auto NewInit = llvm::ConstantStruct::get(
            cast<llvm::StructType>(OldInit->getType()), NewInitElems);
        VTable->setInitializer(NewInit);
    }
};

// Emit the modified vtbl for the most derived C++ class
void LangPlugin::emitAdditionalClassSymbols(::ClassDeclaration *cd)
{
    if (!getASTUnit())
        return; // no C++ class around

    if (!isDCXX(cd))
        return;
    auto dcxxInfo = DCXXVTableInfo::get(cd);

    auto& Context = getASTContext();
    auto VTContext = Context.getVTableContext();
    
    if (!VTContext->isMicrosoft()) {
        auto RTTI = CGM->GetAddrOfRTTIDescriptor(
            Context.getTagDeclType(dcxxInfo->MostDerivedBase));

        auto vtableZ = getDCXXVTable(cd, dcxxInfo);
        vtableZ->setLinkage(DtoLinkage(cd).first);

        DCXXVTableAdjuster<clang::ItaniumVTableBuilder> VTAdjuster(cd, *dcxxInfo, *CGM, Context);
        VTAdjuster.adjustVTableInitializer(vtableZ, *dcxxInfo->VTLayout, RTTI);
    } else {
        clang::MicrosoftVTableContext &VFTContext = CGM->getMicrosoftVTableContext();
        const clang::VPtrInfoVector &VFPtrs = VFTContext.getVFPtrOffsets(dcxxInfo->MostDerivedBase);

        for (auto& Info : VFPtrs) {
            const clang::VTableLayout &VTLayout =
                VFTContext.getVFTableLayout(dcxxInfo->MostDerivedBase, Info->FullOffsetInMDC);

            llvm::Constant *RTTI = nullptr;
            if (any_of(VTLayout.vtable_components(),
                [](const clang::VTableComponent &VTC) { return VTC.isRTTIKind(); }))
                RTTI = clangCG::getMSCompleteObjectLocator(CGM->getCXXABI(),
                                dcxxInfo->MostDerivedBase, *Info.get());

            auto vtableZ = getDCXXVTable(cd, dcxxInfo, Info->FullOffsetInMDC, &VTLayout);
            vtableZ->setLinkage(DtoLinkage(cd).first);

            DCXXVTableAdjuster<clang::VFTableBuilder> VTAdjuster(cd, *dcxxInfo, *CGM, Context);
            VTAdjuster.adjustVTableInitializer(vtableZ, VTLayout, RTTI, Info.get());
        }
    }
}

// A few changes to CGClass.cpp here and there
// TODO: Now that Clang is a submodule, trim down redundant code
struct DCXXVptrAdjuster
{
    clangCG::CodeGenModule &CGM;
    clangCG::CodeGenFunction &CGF;
    llvm::IRBuilder<> &Builder;

    ::ClassDeclaration *cd;
    DCXXVTableInfo& dcxxInfo;
    llvm::Value *cxxThis;

    inline clang::ASTContext& getContext() {
        return calypso.getASTContext();
    }

    DCXXVptrAdjuster(clangCG::CodeGenModule &CGM,
            llvm::Value *cxxThis, ::ClassDeclaration *cd, DCXXVTableInfo& dcxxInfo)
        : CGM(CGM),
          CGF(*calypso.CGF()),
          Builder(gIR->scope().builder),
          cd(cd),
          dcxxInfo(dcxxInfo),
          cxxThis(cxxThis)
    {}

    llvm::Value *
    ApplyNonVirtualAndVirtualOffset(llvm::Value *ptr,
                                    clang::CharUnits nonVirtualOffset,
                                    llvm::Value *virtualOffset)
    {
        // Assert that we have something to do.
        assert(!nonVirtualOffset.isZero() || virtualOffset != nullptr);

        // Compute the offset from the static and dynamic components.
        llvm::Value *baseOffset;
        if (!nonVirtualOffset.isZero()) {
            baseOffset = llvm::ConstantInt::get(DtoType(Type::tptrdiff_t),
                                                nonVirtualOffset.getQuantity());
            if (virtualOffset) {
            baseOffset = Builder.CreateAdd(virtualOffset, baseOffset);
            }
        } else {
            baseOffset = virtualOffset;
        }

        // Apply the base offset.
        ptr = Builder.CreateBitCast(ptr, DtoType(Type::tint8->pointerTo()));
        ptr = Builder.CreateInBoundsGEP(ptr, baseOffset, "add.ptr");
        return ptr;
    }

    clangCG::Address GetCXXThisAddress(llvm::Value* cxxThis) {
        // Just use the best known alignment for the parent.
        auto CXXThisAlignment = CGM.getClassPointerAlignment(dcxxInfo.MostDerivedBase);
        return clangCG::Address(cxxThis, CXXThisAlignment);
    }

    void
    InitializeVTablePointer(clang::BaseSubobject Base,
                                            const clang::CXXRecordDecl *NearestVBase,
                                            clang::CharUnits OffsetFromNearestVBase,
                                            const clang::CXXRecordDecl *VTableClass)
    {
        // Compute the address point.
        llvm::Value *VTableAddressPoint =
            CGM.getCXXABI().getVTableAddressPointInStructor(
                CGF, VTableClass, Base, NearestVBase);
        if (!VTableAddressPoint)
            return;

        // And now the CALYPSO trick
        clang::CharUnits DCXXOffsetSuffix;
        const clang::VTableLayout* VTLayout = nullptr;

        if (getContext().getVTableContext()->isMicrosoft()) {
            DCXXOffsetSuffix = Base.getBaseOffset();
            VTLayout = &CGM.getMicrosoftVTableContext().getVFTableLayout(
                            dcxxInfo.MostDerivedBase, DCXXOffsetSuffix);
        }

        auto DCXXVTable = getDCXXVTable(cd, nullptr, DCXXOffsetSuffix, VTLayout);
        llvm::Value* DCXXVTableAddressPoint;

        if (!getContext().getVTableContext()->isMicrosoft()) {
            auto ConstGEP = llvm::cast<llvm::ConstantExpr>(VTableAddressPoint);

            llvm::Constant *Indices[] = {
                ConstGEP->getOperand(1),
                ConstGEP->getOperand(2),
                ConstGEP->getOperand(3)
            };

            DCXXVTableAddressPoint =
                llvm::ConstantExpr::getGetElementPtr(DCXXVTable->getValueType(), DCXXVTable,
                                              Indices, /*InBounds=*/true,
                                              /*InRangeIndex=*/1);
        } else {
            if (getContext().getLangOpts().RTTIData)
                DCXXVTableAddressPoint = Builder.CreateConstInBoundsGEP2_64(DCXXVTable, 0, 1);
            else
                DCXXVTableAddressPoint = DCXXVTable;
        }

        // Compute where to store the address point.
        llvm::Value *VirtualOffset = nullptr;
        clang::CharUnits NonVirtualOffset = clang::CharUnits::Zero();

        clangCG::CodeGenFunction::VPtr Vptr = { Base, NearestVBase, OffsetFromNearestVBase, VTableClass };
        if (CGM.getCXXABI().isVirtualOffsetNeededForVTableField(CGF, Vptr)) {
            // We need to use the virtual base offset offset because the virtual base
            // might have a different offset in the most derived class.
            VirtualOffset = CGM.getCXXABI().GetVirtualBaseClassOffset(CGF,
                                                                    GetCXXThisAddress(cxxThis),
                                                                    VTableClass,
                                                                    NearestVBase);
            NonVirtualOffset = OffsetFromNearestVBase;
        } else {
            // We can just use the base offset in the complete class.
            NonVirtualOffset = Base.getBaseOffset();
        }

        // Apply the offsets.
        llvm::Value *VTableField = cxxThis;

        if (!NonVirtualOffset.isZero() || VirtualOffset)
            VTableField = ApplyNonVirtualAndVirtualOffset(VTableField,
                                                        NonVirtualOffset,
                                                        VirtualOffset);

        // Finally, store the address point.
        llvm::Type *AddressPointPtrTy =
            DCXXVTableAddressPoint->getType()->getPointerTo();
        VTableField = Builder.CreateBitCast(VTableField, AddressPointPtrTy);
        /* llvm::StoreInst *Store = */ Builder.CreateStore(DCXXVTableAddressPoint, VTableField);
//         CGM.DecorateInstruction(Store, CGM.getTBAAInfoForVTablePtr());
    }

    void
    InitializeVTablePointers(clang::BaseSubobject Base,
                                            const clang::CXXRecordDecl *NearestVBase,
                                            clang::CharUnits OffsetFromNearestVBase,
                                            bool BaseIsNonVirtualPrimaryBase,
                                            const clang::CXXRecordDecl *VTableClass,
                                            clangCG::CodeGenFunction::VisitedVirtualBasesSetTy& VBases)
    {
        // If this base is a non-virtual primary base the address point has already
        // been set.
        if (!BaseIsNonVirtualPrimaryBase) {
            // Initialize the vtable pointer for this base.
            InitializeVTablePointer(Base, NearestVBase, OffsetFromNearestVBase,
                                    VTableClass);
        }

        const clang::CXXRecordDecl *RD = Base.getBase();

        // Traverse bases.
        for (const auto &I : RD->bases()) {
            clang::CXXRecordDecl *BaseDecl
            = llvm::cast<clang::CXXRecordDecl>(I.getType()->getAs<clang::RecordType>()->getDecl());

            // Ignore classes without a vtable.
            if (!BaseDecl->isDynamicClass())
            continue;

            clang::CharUnits BaseOffset;
            clang::CharUnits BaseOffsetFromNearestVBase;
            bool BaseDeclIsNonVirtualPrimaryBase;

            if (I.isVirtual()) {
            // Check if we've visited this virtual base before.
            if (!VBases.insert(BaseDecl).second)
                continue;

            const clang::ASTRecordLayout &Layout =
                getContext().getASTRecordLayout(VTableClass);

            BaseOffset = Layout.getVBaseClassOffset(BaseDecl);
            BaseOffsetFromNearestVBase = clang::CharUnits::Zero();
            BaseDeclIsNonVirtualPrimaryBase = false;
            } else {
            const clang::ASTRecordLayout &Layout = getContext().getASTRecordLayout(RD);

            BaseOffset = Base.getBaseOffset() + Layout.getBaseClassOffset(BaseDecl);
            BaseOffsetFromNearestVBase =
                OffsetFromNearestVBase + Layout.getBaseClassOffset(BaseDecl);
            BaseDeclIsNonVirtualPrimaryBase = Layout.getPrimaryBase() == BaseDecl;
            }

            InitializeVTablePointers(clang::BaseSubobject(BaseDecl, BaseOffset),
                                    I.isVirtual() ? BaseDecl : NearestVBase,
                                    BaseOffsetFromNearestVBase,
                                    BaseDeclIsNonVirtualPrimaryBase,
                                    VTableClass, VBases);
        }
    }
};

// Adjust the C++ vptrs of a newly constructed DCXX class
void LangPlugin::toPostNewClass(Loc& loc, TypeClass* tc, DValue* val)
{
    if (!getASTUnit())
        return;

    auto cd = static_cast<::ClassDeclaration*>(tc->sym);

    if (!isDCXX(cd))
        return;
    auto dcxxInfo = DCXXVTableInfo::get(cd);

    auto cxxThis = DtoCast(loc, val,
                           dcxxInfo->mostDerivedCXXBase->type->pointerTo());
    DCXXVptrAdjuster adjuster(*CGM, DtoRVal(cxxThis), cd, *dcxxInfo);

    auto RD = dcxxInfo->mostDerivedCXXBase->RD;

    // HACK HACK UGLY but... just try to comment
    for (auto MD: dcxxInfo->mostDerivedCXXBase->RD->methods()) {
        if (!llvm::isa<clang::CXXConstructorDecl>(MD) && !llvm::isa<clang::CXXDestructorDecl>(MD)) {
            CGF()->CurGD = MD;
            break;
        }
    }

    // Initialize the vtable pointers for this class and all of its bases.
    clangCG::CodeGenFunction::VisitedVirtualBasesSetTy VBases;
    adjuster.InitializeVTablePointers(clang::BaseSubobject(RD, clang::CharUnits::Zero()),
                            /*NearestVBase=*/nullptr,
                            /*OffsetFromNearestVBase=*/clang::CharUnits::Zero(),
                            /*BaseIsNonVirtualPrimaryBase=*/false, RD, VBases);
}

}
