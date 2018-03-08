// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::string example.
 */

module _string_;

pragma (cppmap, "<string>");

import std.stdio, std.conv;
import (C++) std.string : cppstring = string;

void main()
{
    cppstring s;

    immutable char[] charArray = "Haumea";
    s.reserve(charArray.length * 2);
    s.insert(0, charArray.ptr, charArray.length);

    writeln(s.c_str.to!string);
    // CHECK: Haumea
    writeln("3rd and 5th characters: ", s[2], ", ", s[4]);
    // CHECK: 3rd and 5th characters: u, e
    writeln("Size: ", s.size);
    // CHECK: Size: 6
    writeln("Capacity: ", s.capacity);
    // CHECK: Capacity: 15

    auto s2 = cppstring("Hi'iaka");
    s += ',';
    s.push_back(' ');
    s += s2;

    s.insert(0, "< ");
    s += " >".ptr;
    writeln(s.c_str.to!string);
    // CHECK: < Haumea, Hi'iaka >

    s.clear();
}
