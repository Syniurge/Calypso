/**
 * C++ catch basic examples.
 *
 * Build with :
 *   $ clang++ -c std_exception_cpp.cpp
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
    // We should be falling through to the last catch
    try
    {
        writeln("Throwing an ooops exception");
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
        writefln("Catching the ooops, e.what() == %s", to!string(e.what()));
    }

    // This time with a std::exception catch handling any class derived from it
    try
    {
        writeln("\nThrowing another ooops exception");
        throwException();
    }
    catch (Throwable t)
    {
        writeln("Nope, catched a Throwable");
    }
    catch (C++) (ooops **a)
    {
        writeln("Wrong catch, this is for ooops**");
    }
    catch (C++) (exception *e) // std::exception
    {
        writefln("Catching the std::exception, e.what() == %s", to!string(e.what()));
    }
}
