// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

modmap (C++) "<cmath>";

import (C++) std._;
import std.stdio : writeln;

void main()
{
    // CHECK: acosh(500) = 6.90775
    writeln("acosh(500) = ", acosh(cast(real) 500));
    // CHECK: sin(0.8159) = 0.728343
    writeln("sin(0.8159) = ", sin(cast(float) 0.8159));
    // CHECK: atan2(-2.0, -2.0) = -2.35619
    writeln("atan2(-2.0, -2.0) = ", atan2(cast(double) -2.0, cast(double) -2.0));
}
