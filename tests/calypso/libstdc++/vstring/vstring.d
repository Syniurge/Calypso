/**
 * std::vstring example.
 *
 * Build with:
 *   $ ldc2 vstring.d
 */

modmap (C++) "<vector>";
modmap (C++) "<string>";

import std.stdio;
import (C++) std.vector;
import (C++) std.basic_string;
import (C++) std._ : cppstring = string;

void main()
{
    auto v = new vector!char;
    auto s = new cppstring;
    auto vs = new vector!(cppstring);

    v.reserve(10);
    writeln("vstring apppears to work");
}