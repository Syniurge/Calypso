/**
 * std::foreach example.
 *
 * Build with:
 *   $ ldc2 foreach.d
 */

modmap (C++) "<algorithm>";
modmap (C++) "<vector>";

import std.stdio;
import (C++) std.vector;
import (C++) std._ : Foreach = for_each;

void main()
{
    auto v = new vector!int;
    writeln("foreach compiles");

    // To be done
}