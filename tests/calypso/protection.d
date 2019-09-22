// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// R_N: %t > %t.out
// R_N: FileCheck %s < %t.out

pragma (cppmap, "protection.h");

import (C++) Base33641;

class Derived21544 : Base33641
{
//     void bar5432() { this.Base33641.n = 84; } // DMD BUG?: this doesn't work
    void bar5432() { this.n = 84; }
}

void main()
{
    Derived21544 s;
    s.bar5432();
}
