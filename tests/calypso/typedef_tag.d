// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// R_N: %t > %t.out
// R_N: FileCheck %s < %t.out

pragma (cppmap, "typedef_tag.h");

import (C++) ABC;
import (C++) RenamedAnon = AnonEnum;
static import (C++) AnonStruct;

struct S {
    import (C++) XYZ;
    XYZ x;
}

void main()
{
    ABC a;
    {
        import (C++) XYZ;
        XYZ x;
    }
    {
        import (C++) _;
        XYZ x;
    }

    RenamedAnon.AnonEnum e = RenamedAnon.AnonEnum.OOOO;
    ℂcpp.AnonStruct.AnonStruct s;
}
