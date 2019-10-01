// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// R_N: %t > %t.out
// R_N: FileCheck %s < %t.out

pragma (cppmap, "reference_types.h");

import (C++) ZString, _;

void main()
{
    Foo6520(ZString("XYZ"));
}
