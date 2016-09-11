/**
 * std::complex example.
 *
 * Build with:
 *   $ ldc2 complex.d
 */

modmap (C++) "<complex>";

import std.stdio, std.conv, std.string;
import (C++) std._ : cppcomplex = complex;

void main()
{
    auto v = cppcomplex!(float)(1.2,3.4);
}