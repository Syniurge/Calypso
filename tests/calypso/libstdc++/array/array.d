/**
 * std::array example.
 *
 * Build with:
 *   $ ldc2 -cpp-args -std=c++11 array.d
 */

module _array_;

modmap (C++) "<array>";

import std.stdio, std.conv, std.string;
import (C++) std.array;

void main()
{
    auto v = new array!(char, 10);
    writeln("array");
}