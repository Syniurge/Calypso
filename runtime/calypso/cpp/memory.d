/**
 * This module provides two utility functions to allocate and construct C++ class or struct objects without the GC.
 *
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Elie Morisse
 */

 module cpp.memory;

/**
    When a C++ library expects to be granted ownership of a piece of memory, the allocation shouldn't be done by the GC
    unless you always keep a reference to the allocated memory, which is pointless additional work and not always possible.
    Adapted from: https://wiki.dlang.org/Memory_Management#Explicit_Class_Instance_Allocation
*/
T* cppNew(T, Args...) (Args args)
{
    import core.stdc.stdlib : malloc;

    // get size of aggregate instance in bytes
    static if ( is(T == class) || is(T == struct) )
        auto size = T.sizeof;
    else
        static assert(false);

    // allocate memory for the object
    auto memory = malloc(size)[0..size];
    if (!memory) {
        import core.exception : onOutOfMemoryError;
        onOutOfMemoryError();
    }

    // call T's constructor and emplace instance on newly allocated memory
    auto result = cast(T*) memory.ptr;
    static if (!__traits(hasMember, T, "__ctor"))
        static assert(!Args.length);
    else
        result.__ctor(args);
    return result;
}

void cppDelete(T)(T obj)
{
    import core.stdc.stdlib : free;

    // calls obj's destructor
    static if (__traits(hasMember, T, "__dtor"))
        obj.__dtor();

    // free memory occupied by object
    free(cast(void*)obj);
}
