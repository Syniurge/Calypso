// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "functemplateoverloads.h");

import (C++) _;

void main()
{
    ushort us = 6;
    ubyte ub = foo!ubyte(us);
}
