// RUN: mkdir %t.cache; \
// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "functemplateoverloads.h";

import (C++) _;

void main()
{
    ushort us = 6;
    ubyte ub = foo!ubyte(us);
}
