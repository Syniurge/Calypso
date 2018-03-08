// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "explicitly_implicit_ctor.h");

import (C++) S, _;

void main()
{
    foo(4268);
}
