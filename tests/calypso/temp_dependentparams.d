// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "temp_dependentparams.h";

import (C++) Slong, A;

void main()
{
    A!(Slong, 9) s;
}
