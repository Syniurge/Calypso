// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

pragma (cppmap, "eh_catch.h");

import (C++) std.exception, ooops;
import (C++) test._;
import std.stdio, std.conv;

void main()
{
    // We should be falling through to the last catch
    // CHECK: Catching the ooops, e.what() == Ooops!
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
        writefln("Catching the ooops, e.what() == %s", e.what().to!string);
    }

    // This time with a std::exception catch handling any class derived from it
    // CHECK: Catching the std::exception, e.what() == Ooops!
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
        writefln("Catching the std::exception, e.what() == %s", e.what().to!string);
    }

    // Let's try throwing a basic type
    // CHECK: Catching the float, f == 20.16
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
