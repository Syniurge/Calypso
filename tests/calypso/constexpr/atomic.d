modmap (C++) "<atomic>";

import std.stdio, std.conv;
import (C++) std.atomic;
import (C++) std._ : atomic_load;

immutable atomic!int x = atomic!int(987654);

void main()
{
    writeln("x = ", atomic_load(&x));
}
