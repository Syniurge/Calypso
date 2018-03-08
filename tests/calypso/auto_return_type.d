// RUN: %ldc -cpp-args -std=c++14 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

pragma (cppmap, "auto_return_type.h");

import (C++) _;
import std.stdio : writeln;

void main() {
    auto a = _tims_my_def2(3);
    static assert(is(typeof(a) == int));

    writeln("a = ", a);
    // CHECK: a = 6
}
