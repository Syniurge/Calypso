//===-- irtypestruct.cpp --------------------------------------------------===//
//
//                         LDC – the LLVM D compiler
//
// This file is distributed under the BSD-style LDC license. See the LICENSE
// file for details.
//
//===----------------------------------------------------------------------===//

#include "ir/irtypestruct.h"

#include "llvm/IR/DerivedTypes.h"

#include "aggregate.h"
#include "declaration.h"
#include "init.h"
#include "mtype.h"
#include "import.h"

#include "gen/irstate.h"
#include "gen/tollvm.h"
#include "gen/logger.h"
#include "gen/llvmhelpers.h"
#include "gen/cgforeign.h"

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

IrTypeStruct::IrTypeStruct(StructDeclaration *sd)
    : IrTypeAggr(sd), sd(sd), ts(static_cast<TypeStruct *>(sd->type)) {}

//////////////////////////////////////////////////////////////////////////////

IrTypeStruct *IrTypeStruct::get(StructDeclaration *sd) {
  auto t = new IrTypeStruct(sd);
  sd->type->ctype = t;

  IF_LOG Logger::println("Building struct type %s @ %s", sd->toPrettyChars(),
                         sd->loc.toChars());
  LOG_SCOPE;

  // if it's a forward declaration, all bets are off, stick with the opaque
  if (sd->sizeok != SIZEOKdone) {
    return t;
  }

  // CALYPSO
  if (auto lp = sd->langPlugin())
    t->type = lp->codegen()->toType(sd->type);
          // what about default_fields
  else
    for (auto lp: global.langPlugins)
      if (auto Ty = lp->codegen()->IrTypeStructHijack(sd)) // CALYPSO wonderful HACK for __cpp_member_ptr
        t->type = Ty;

  auto StructTy = llvm::cast<LLStructType>(t->type);
  if (!StructTy->isOpaque())
  {
    t->packed = StructTy->isPacked();
    return t;
  }

  t->packed = sd->alignment == 1;
  if (!t->packed) {
    // Unfortunately, the previous check is not enough in case the struct
    // contains an align declaration. See issue 726.
    t->packed = isPacked(sd);
  }

  AggrTypeBuilder builder(t->packed);
  builder.addAggregate(sd);
  builder.addTailPadding(sd->structsize);
  isaStruct(t->type)->setBody(builder.defaultTypes(), t->packed);
  t->varGEPIndices = builder.varGEPIndices();

  IF_LOG Logger::cout() << "final struct type: " << *t->type << std::endl;

  return t;
}
