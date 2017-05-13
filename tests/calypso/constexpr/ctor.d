modmap (C++) "ctor.hpp";

import std.stdio, std.conv;
import (C++) A;

immutable A xyz = A(3);

void main()
{
    writeln("xyz = ", xyz.n, " ", xyz.f);
}
