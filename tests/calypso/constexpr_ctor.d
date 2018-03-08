// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

pragma (cppmap, "constexpr_ctor.hpp");

import std.stdio : writeln;
import (C++) A;

immutable A xyz = A(3);

void main()
{
    // CHECK: xyz = 3, 9.81
    writeln("xyz = ", xyz.n, ", ", xyz.f);
}
