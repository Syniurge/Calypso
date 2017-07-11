// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

modmap (C++) "<atomic>";

import std.stdio : writeln;
import (C++) std.atomic;
import (C++) std._ : atomic_load;

immutable atomic!int x = atomic!int(987654);
immutable atomic!int y = atomic!int(654321);

void main()
{
    // CHECK: x = 987654
    writeln("x = ", atomic_load(&x));
    // CHECK: y = 654321
    writeln("y = ", atomic_load(&y));
}
