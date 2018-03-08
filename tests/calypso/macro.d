// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

pragma (cppmap, "macro.h");

import (C++) _;
import std.stdio : writeln;

void main()
{
    writeln("my_macro = ", my_macro);
    // CHECK: my_macro = 42

    writeln("empty_macro = ", empty_macro);
    // CHECK: empty_macro = true
}
