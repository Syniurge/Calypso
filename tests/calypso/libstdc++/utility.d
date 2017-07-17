// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::utility example.
 */

modmap (C++) "<utility>";

import std.stdio;
import (C++) std.pair;
import (C++) std._ : make_pair;

void main()
{
    auto p = make_pair!(int, int)(10, 20);
    writeln("first in pair = ", p.first);
    // CHECK: first in pair = 10
    writeln("second in pair = ", p.second);
    // CHECK: second in pair = 20
}
