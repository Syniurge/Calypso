/**
 * Compiler implementation of the D programming language
 * http://dlang.org
 *
 * Copyright: Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, http://www.digitalmars.com
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/rootobject.d, root/_rootobject.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_rootobject.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/rootobject.d
 */

module dmd.root.rootobject;

import core.stdc.stdio;

import dmd.root.outbuffer;

/***********************************************************
 */

enum DYNCAST : int
{
    object,
    expression,
    dsymbol,
    type,
    identifier,
    tuple,
    parameter,
    statement,
    condition,
    templateparameter,
}

/***********************************************************
 */

extern (C++) class RootObject
{
    this() nothrow pure @nogc @safe
    {
    }

    void _key(); // CALYPSO

    bool equals(RootObject o)
    {
        return o is this;
    }

    const(char)* toChars()
    {
        assert(0);
    }

    ///
    extern(D) const(char)[] toString()
    {
        import core.stdc.string : strlen;
        auto p = this.toChars();
        return p[0 .. strlen(p)];
    }

    // CALYPSO HACK: emit toString() mangled as a C++ method to make the vtable generation
    // possible by C++ compilers. This is the function that ends up in vtables of C++
    // classes derived from D ones.
    // It can't be rewritten in a .cpp file as DString toString() because on some platforms (e.g MSVC) the ABI doesn't match
    version(CRuntime_Microsoft)
        enum toString_cppmangling = "?toString@RootObject@@UEAA?AUDString@@XZ";
    else
        enum toString_cppmangling = "_ZN10RootObject8toStringEv";
    pragma(mangle, toString_cppmangling)
    final extern(D) const(char)[] toString_()
    {
        import core.stdc.string : strlen;
        auto p = this.toChars();
        return p[0 .. strlen(p)];
    }

    DYNCAST dyncast() const nothrow pure @nogc @safe
    {
        return DYNCAST.object;
    }
}
