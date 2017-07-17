// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::map example.
 */

module _map_;

modmap (C++) "<map>";
modmap (C++) "<string>";

import std.stdio, std.conv;
import (C++) std.map;
import (C++) std._ : cppstring = string;

void main()
{
    map!(char, cppstring) m;

    immutable char a = '0';
    immutable char b = 'z';
    m[a] = cppstring("Sedna");
    m[b] = "90377";

    writeln(m['z'].c_str.to!string, " ", m['0'].c_str.to!string);
    // CHECK: 90377 Sedna
}
