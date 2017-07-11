// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "explicitly_implicit_ctor.h";

import (C++) S, _;

void main()
{
    foo(4268);
}
