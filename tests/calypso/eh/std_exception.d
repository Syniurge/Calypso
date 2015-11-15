/**
 * C++ catch basic examples.
 *
 * Build with :
 *   $ clang++ -c std_exception_cpp.cpp
 *   $ ldc2 std_exception_cpp.o -L-lstdc++ std_exception.d
 */

modmap (C++) "std_exception_cpp.h";

import (C++) std.exception, ooops;
import (C++) test._;
import std.stdio, std.conv, std.string;

void main()
{
    // We should be falling through to the last catch
    try
    {
        writeln("Throwing an ooops exception");
        throwOoops();
        writeln("Something's wrong... resuming 1st try instead of unwinding?");
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
    catch (C++) (ref ooops e)
    {
        writefln("Catching the ooops, e.what() == %s", to!string(e.what()));
    }

    // This time with a std::exception catch handling any class derived from it
    try
    {
        writeln("\nThrowing another ooops exception");
        throwOoops();
        writeln("Something's wrong... resuming 2nd try instead of unwinding?");
    }
    catch (Throwable t)
    {
        writeln("Nope, catched a Throwable");
    }
    catch (C++) (ooops *a)
    {
        writeln("Wrong catch, this is for ooops*");
    }
    catch (C++) (ref exception e) // std::exception
    {
        writefln("Catching the std::exception, e.what() == %s", to!string(e.what()));
    }

    // Let's try throwing a basic type
    try
    {
        writeln("\nNow throwing a float");
        throwFloat();
        writeln("Something's wrong... resuming 3rd try instead of unwinding?");
    }
    catch (Throwable t)
    {
        writeln("Nope, catched a Throwable");
    }
    catch (C++) (float f)
    {
        writefln("Catching the float, f == %f!", f);
    }
    catch (C++) (ref exception e)
    {
        writeln("Wrong catch, this is for std::exception&");
    }
}
