// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPSTATEMENT_H
#define DMD_CPP_CPPSTATEMENT_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "statement.h"
#include "cpp/calypso.h"

namespace cpp
{
struct IrCatch;

class Catch : public ::Catch
{
public:
    CALYPSO_LANGPLUGIN

    StorageClass stc;

    Catch(Loc loc, Type *t, Identifier *id, Statement *handler, StorageClass stc);
    VarDeclaration *createVar() override;
    ::Catch *syntaxCopy() override;
    
    bool onlyCatchThrowable() override { return false; }

    IrCatch *ir = nullptr;
};

}

#endif