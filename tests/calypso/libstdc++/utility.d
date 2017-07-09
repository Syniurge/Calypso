/**
 * std::utility example.
 *
 * Build with:
 *   $ ldc2 utility.d
 */

modmap (C++) "<utility>";

import std.stdio;
import (C++) std.pair;
import (C++) std._ : stlmakepair = make_pair;

void main()
{
    writeln("utility compiles");

    auto p = stlmakepair!(int,int)(10,20);
    writeln("first in pair = ", p.first);
    writeln("second in pair = ", p.second);
}
