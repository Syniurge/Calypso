modmap (C++) "factorial.hpp";

import std.stdio, std.conv;
import (C++) _ : factorial;

immutable int[] fact = [ factorial(1), factorial(2), factorial(3), factorial(4), factorial(5) ];

void main()
{
    foreach(n; fact)
        writeln(n);
}
