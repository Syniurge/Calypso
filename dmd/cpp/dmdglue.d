// Contributed by Elie Morisse, same license DMD uses

import dmd.dsymbol;
import dmd.func;

extern (C++) int overloadApply(Dsymbol fstart, void* param, int function(void*, Dsymbol) fp)
{
    return dmd.func.overloadApply(fstart, s => (*fp)(param, s));
}

