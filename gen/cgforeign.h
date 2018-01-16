#pragma once

#include "llvm.h"

#include "ddmd/declaration.h"
#include "ir/irtypeaggr.h"

class Type;
class ClassDeclaration;
class StructDeclaration;
class VarDeclaration;
class Catch;

struct IRState;
struct IrAggr;

class ForeignCodeGen
{
public:
    virtual void enterModule(::Module *m, llvm::Module *lm) = 0;
    virtual void leaveModule(::Module *m, llvm::Module *lm) = 0;

    virtual void enterFunc(FuncDeclaration *fd) = 0;
    virtual void leaveFunc() = 0;

    virtual LLType *toType(Type *t) = 0;
    virtual llvm::FunctionType *toFunctionType(FuncDeclaration *fdecl) = 0;
    virtual bool passAggregateArgumentByRef(AggregateDeclaration* ad) = 0;
    virtual llvm::Type *IrTypeStructHijack(StructDeclaration *sd) = 0; // UGLY HACK

    virtual llvm::Constant *createInitializerConstant(IrAggr *irAggr,
        const std::map<VarDeclaration *, llvm::Constant *>& explicitInitializers) = 0;
    virtual llvm::Constant *createStructLiteralConstant(StructLiteralExp *e) = 0;
    virtual llvm::Constant *toConstElemFallback(Expression *e) = 0;
    virtual bool addFieldInitializers(llvm::SmallVectorImpl<llvm::Constant*>& constants,
            const std::map<VarDeclaration *, llvm::Constant *>& explicitInitializers, AggregateDeclaration* decl,
            unsigned& offset, bool populateInterfacesWithVtbls) = 0; // used for "hybrid" classes i.e D classes inheriting from foreign ones

    virtual void toResolveFunction(FuncDeclaration* fdecl) = 0;
    virtual void toDefineFunction(FuncDeclaration* fdecl) = 0;
    virtual void toDeclareVariable(VarDeclaration* vd) = 0;
    virtual void toDefineVariable(VarDeclaration* vd) = 0;
    virtual void toDefineStruct(StructDeclaration* sd) = 0;
    virtual void toDefineClass(ClassDeclaration* cd) = 0;

    virtual bool toIsReturnInArg(CallExp* ce) = 0;
    virtual LLValue *toVirtualFunctionPointer(DValue* inst, FuncDeclaration* fdecl, const char* name) = 0;
    virtual DValue* toCallFunction(Loc& loc, Type* resulttype, DValue* fnval,
                                   const std::vector<DValue *> &argvals, llvm::Value *retvar) = 0;

    virtual bool toConstructVar(VarDeclaration *vd, llvm::Value *value, Expression *rhs) = 0;

    virtual LLValue* toIndexAggregate(LLValue* src, AggregateDeclaration* ad, VarDeclaration* vd, Type *srcType) = 0;
    virtual void addBaseClassData(AggrTypeBuilder &builder, AggregateDeclaration *base) = 0;
    virtual void emitAdditionalClassSymbols(ClassDeclaration *cd) = 0;
    virtual void toInitClass(TypeClass* tc, LLValue* dst) = 0;
    virtual DValue *toDynamicCast(Loc &loc, DValue *val, Type *_to) = 0;
    virtual DValue *adjustForDynamicCast(Loc &loc, DValue *val, Type *_to) = 0;

    // Called for any aggregate (TODO: less ambiguous names?)
    virtual void toPostNewClass(Loc& loc, TypeClass* tc, DValue* val) = 0;

    // Exception handling
    virtual void toBeginCatch(IRState& irs, Catch *cj) = 0;
    virtual void toEndCatch(IRState& irs, Catch *cj) = 0;
    virtual llvm::GlobalVariable* toCatchScopeType(IRState& irs, Type *t) = 0;
};
