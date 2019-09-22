// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// R_N: %t > %t.out
// R_N: FileCheck %s < %t.out

pragma (cppmap, "anon_tag.h");

import (C++) S825;

void main()
{
    S825 s;
}
