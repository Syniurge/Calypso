// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

pragma(cppmap, "record_func_params.h");

import (C++) *;
import std.stdio : writeln;

// TODO: check that extern(C++) functions match the expected C++ ABI?

void testStructS(TestStructS s)
{
    writeln("StructS");
    writeln("a = ", s.a);
    writeln("");
}

void testStructM(TestStructM s)
{
    writeln("StructM");
    writeln("a = ", s.a,
            ", b = ", s.b,
            ", c = ", s.c);
    writeln("");
}

void testStructL(TestStructL s)
{
    writeln("StructL");
    writeln("a = ", s.a,
            ", b = ", s.b,
            ", c = ", s.c,
            ", d = ", s.d);
    writeln("");
}

void testStructXL(TestStructXL s)
{
    writeln("StructXL");
    writeln("a = ", s.a,
            ", b = ", s.b,
            ", c = ", s.c,
            ", d = ", s.d,
            ", e = ", s.e);
    writeln("");
}

void testStructFloatS(TestStructFloatS s)
{
    writeln("StructFloatS");
    writeln("a = ", s.a);
    writeln("");
}

void testStructFloatM(TestStructFloatM s)
{
    writeln("StructFloatM");
    writeln("a = ", s.a,
            ", b = ", s.b,
            ", c = ", s.c);
    writeln("");
}

void testStructMixed(TestStructMixed s)
{
    writeln("StructMixed");
    writeln("a = ", s.a,
            ", b = ", s.b,
            ", c = ", s.c,
            ", d = ", s.d);
    writeln("");
}

void testDC(DynamicClass c)
{
    c.makeMeDynamic();
}

void main()
{
    TestStructS s1 = { 38 };
    testStructS(s1);
    // CHECK: StructS
    // CHECK: a = 38

    TestStructM s2 = { 21, 735, 50000 };
    testStructM(s2);
    // CHECK: StructM
    // CHECK: a = 21, b = 735, c = 50000

    TestStructL s3 = { 85, 434, 90000, 222222 };
    testStructL(s3);
    // CHECK: StructL
    // CHECK: a = 85, b = 434, c = 90000, d = 222222

    TestStructXL s4 = { 13, 838, 10001, 747474, 8513214 };
    testStructXL(s4);
    // CHECK: StructXL
    // CHECK: a = 13, b = 838, c = 10001, d = 747474, e = 8513214

    TestStructFloatS s5 = { 40.0 };
    testStructFloatS(s5);
    // CHECK: StructFloatS
    // CHECK: a = 40

    TestStructFloatM s6 = { 78.0, 548.24, 963.14 };
    testStructFloatM(s6);
    // CHECK: StructFloatM
    // CHECK: a = 78, b = 548.24, c = 963.14

    TestStructMixed s7 = { 42.5555, 200000000000, 521.9, 500 };
    testStructMixed(s7);
    // CHECK: StructMixed
    // CHECK: a = 42.5555, b = 200000000000, c = 521.9, d = 500
}

