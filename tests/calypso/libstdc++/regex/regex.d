/**
 * std::regex example.
 *
 * Build with:
 *   $ ldc2 -cpp-args -std=c++11 regex.d
 */

modmap (C++) "<regex>";
modmap (C++) "<string>";

import std.string;
import (C++) std.regex;
import (C++) std.basic_string;
import (C++) std._ : cppstring = string;

void main()
{
    
}