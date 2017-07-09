/**
 * std::algorithm example.
 *
 * Build with:
 *   $ ldc2 future.d
 */

modmap (C++) "<future>";

import std.stdio;
import (C++) std.future;

void main()
{
    writeln("future parsed");
}
