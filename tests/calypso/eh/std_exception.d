/**
 * C++ catch basic example.
 *
 * Build with :
 *   $ clang++ std_exception_cpp.cpp
 *   $ ldc2 std_exception_cpp.o -L-lstdc++ std_exception.d
 */

modmap (C++) "<exception>";
modmap (C++) "std_exception_cpp.h";

import (C++) std.exception;
import (C++) ooops;
import std.stdio, std.conv, std.string;

extern(C++) void throwException();

void main()
{
    try
    {
        writeln("Calling throwException");
        throwException();
    }
    catch (Throwable t)
    {
        writeln("Nope, catched a Throwable");
    }
    catch (C++) (uint n)
    {
        writeln("Wrong catch, this is for C++ uint exceptions");
        writeln("n == ", n);
    }
    catch (C++) (ooops *e)
    {
        writeln("Catching the C++ std::exception!");
        writeln("e == ", e);
        writeln("what() == ", e.what());
    }
}
