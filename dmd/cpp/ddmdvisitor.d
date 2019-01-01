// Contributed by Elie Morisse, same license DMD uses

import dmd.dsymbolsem;
import dmd.dmangle;
import dmd.semantic2;
import dmd.semantic3;

shared static this()
{
    TI_DsymbolSem1Visitor = cast(void*) typeid(DsymbolSemanticVisitor);
    TI_DsymbolSem2Visitor = cast(void*) typeid(Semantic2Visitor);
    TI_DsymbolSem3Visitor = cast(void*) typeid(Semantic3Visitor);

    TI_Mangler = cast(void*) typeid(Mangler);
}

extern (C++) __gshared:

const(void*) TI_DsymbolSem1Visitor;
const(void*) TI_DsymbolSem2Visitor;
const(void*) TI_DsymbolSem3Visitor;

const(void*) TI_Mangler;
