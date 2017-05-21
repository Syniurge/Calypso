/**
 * std::map example.
 *
 * Build with:
 *   $ ldc2 map.d
 */

module _map_;

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
    m[a] = cppstring("Sedna");
    m[b] = "90377";

    writeln(m[b].c_str.to!string, " ", m[a].c_str.to!string);
}