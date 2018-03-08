// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

pragma (cppmap, "baseclassoffset.h");

import (C++) Cls, ClsB, MulInh;
import std.stdio : writeln;

class A
{
    int lm96;
}

interface I
{
    void zf36();
}

class B : A, I
{
    char c;
    override void zf36() {}
}

void main()
{
    MulInh* a = cast(MulInh*) 0x12345678;
    writeln("A|", cast(void*) cast(Cls*) a - cast(void*) cast(ClsB*) a == ClsB.sizeof);
    // CHECK: A|true

    B b = new B;
    writeln("B|", cast(void*) cast(I) b - cast(void*) b == __traits(getBaseOffset, B, I));
    // CHECK: B|true

    writeln("C|",  __traits(getBaseOffset, MulInh, Cls) == ClsB.sizeof);
    // CHECK: C|true
    writeln("D|", __traits(getBaseOffset, B, A));
    // CHECK: D|0
}
