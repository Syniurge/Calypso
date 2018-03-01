// Contributed by Elie Morisse, same license DMD uses

#pragma once

#include "visitor.h"

struct AA;
struct OutBuffer;

/***********************/

// DMD visitors TypeInfo constants
extern const void* const TI_DsymbolSem1Visitor;
extern const void* const TI_DsymbolSem2Visitor;
extern const void* const TI_DsymbolSem3Visitor;

extern const void* const TI_Mangler;

/***********************/

// Missing C++ header declarations of visitors
class Mangler : public Visitor
{
public:
    AA* types;
    AA* idents;
    OutBuffer* buf;
};
