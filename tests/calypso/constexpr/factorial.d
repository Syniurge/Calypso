modmap (C++) "factorial.hpp";

import std.stdio, std.conv;
import (C++) _ : factorial;

immutable int fact3 = factorial(3);

void main()
{
    writeln("fact3 = ", fact3);
}
