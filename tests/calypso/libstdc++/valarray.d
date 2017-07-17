// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
* std::valarray example.
*/

module _valarray_;

modmap (C++) "<valarray>";

import std.stdio;
import (C++) std.valarray;

void main()
{
    // TODO
    auto v = valarray!int(24);
    writeln("valarray compiles");
    // CHECK: valarray compiles
}
