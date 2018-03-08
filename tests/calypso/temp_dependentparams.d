// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "temp_dependentparams.h");

import (C++) Slong, A;

void main()
{
    A!(Slong, 9) s;
}
