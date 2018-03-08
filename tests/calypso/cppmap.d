// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s -I=%S %S/cppmap2.d
// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s -I=%S %S/cppmap2.d -d-version=CPPMAP_B
// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s -I=%S %S/cppmap2.d -d-version=CPPMAP_B -d-version=CPPMAP_C

pragma (cppmap, "cppmap_a.h");
import (C++) S654;

version (CPPMAP_B)
{
    pragma (cppmap, "cppmap_b.h");
    import (C++) C978;
}

import cppmap2;

void main() {}
