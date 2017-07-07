// RUN: mkdir %t.cache; \
// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

/**
 * std::string example.
 *
 * Build with:
 *   $ ldc2 string.d
 */

module _string_;

modmap (C++) "<string>";

import std.stdio, std.conv, std.string;
import (C++) std.basic_string;
import (C++) std._ : cppstring = string;

void main()
{
    cppstring s; // basic_string!char would do too

    immutable char[] charArray = "Haumea";
    s.reserve(charArray.length * 2);
    s.insert(0, charArray.ptr, charArray.length);

    writeln(to!string(s.c_str));
    writeln("3rd and 5th characters: ", s[2], ", ", s[4]);
    writeln("Size: ", s.size);
    writeln("Capacity: ", s.capacity);

    auto s2 = cppstring("Hi'iaka");
    s.push_back(',');
    s.push_back(' ');
    s += s2;

    s.insert(0, "< ");
    s += " >".ptr;
    writeln(to!string(s.c_str));

    s.clear();
}