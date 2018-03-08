// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

pragma (cppmap, "constexpr_factorial.hpp");

import std.stdio : write;
import (C++) _ : factorial;

immutable int[] fact = [ factorial(1), factorial(2), factorial(3), factorial(4), factorial(5) ];

void main()
{
    // CHECK: 1 2 6 24 120
    foreach(n; fact) {
        write(n, " ");
    }
    write("\n");
}
