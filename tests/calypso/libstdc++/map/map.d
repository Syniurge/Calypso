/**
 * std::map example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ map.d
 */

modmap (C++) "<map>";
modmap (C++) "<string>";

import std.stdio, std.conv, std.string;
import (C++) std.map;
import (C++) std._ : cppstring = string;

void main()
{
    map!(char, cppstring) m;

    immutable char a = '0';
    immutable char b = 'z';
    m[a] = /+cppstring(+/"Sedna"/+)+/; // FIXME copy ctor instead of memcpy
    m[b] = "90377";

    writeln(m[b].c_str.to!string, " ", m[a].c_str.to!string);
}