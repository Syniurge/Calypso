//===-- gen/classes.h - D class code generation -----------------*- C++ -*-===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license. See the LICENSE
// file for details.
//
//===----------------------------------------------------------------------===//
//
// Functions for generating LLVM types and init/TypeInfo/etc. values from D
// class declarations and handling class instance values.
//
//===----------------------------------------------------------------------===//

#ifndef LDC_GEN_CLASSES_H
#define LDC_GEN_CLASSES_H

#include "gen/structs.h"

class AggregateDeclaration;
class ClassDeclaration;
class CtorDeclaration;
class FuncDeclaration;
class NewExp;
class TypeClass;

/// CALYPSO
llvm::Value* DtoClassHandle(DValue* val);
DValue *DtoAggregateDValue(Type *t, llvm::Value *v);
llvm::Type* DtoAggregateHandleType( Type* t );
llvm::Type* DtoClassHandleType(TypeClass *tc);

void DtoResolveAggregate(AggregateDeclaration* ad);

/// Resolves the llvm type for a class declaration
void DtoResolveClass(ClassDeclaration *cd);

/// Provides the llvm declaration for a class declaration
// void DtoDeclareClass(ClassDeclaration* cd);

/// Constructs the constant initializer for a class declaration
// void DtoConstInitClass(ClassDeclaration* cd);

/// Provides the llvm definition for a class declaration
void DtoDefineClass(ClassDeclaration* cd);

/// Builds the initializer of cd's ClassInfo.
/// FIXME: this should be put into IrStruct and eventually IrClass.
llvm::Constant *DtoDefineClassInfo(ClassDeclaration *cd);

DValue *DtoNewClass(Loc &loc, TypeClass *type, NewExp *newexp);
void DtoInitClass(TypeClass *tc, llvm::Value *dst);
void DtoFinalizeClass(Loc &loc, llvm::Value *inst);
void DtoFinalizeScopeClass(Loc &loc, llvm::Value *inst, ClassDeclaration *cd);

DValue *DtoCastClass(Loc &loc, DValue *val, Type *to);
DValue *DtoDynamicCastObject(Loc &loc, DValue *val, Type *to);

DValue *DtoDynamicCastInterface(Loc &loc, DValue *val, Type *to);

llvm::Value *DtoVirtualFunctionPointer(DValue *inst, FuncDeclaration *fdecl,
                                       const char *name);

#endif
