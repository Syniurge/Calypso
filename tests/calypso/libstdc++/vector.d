// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

/**
 * std::vector example.
 *
 * Build with:
 *   $ ldc2 vector.d
 */

module _vector_;

modmap (C++) "<vector>";

import std.stdio, std.conv, std.string;
import (C++) std.vector;

void main()
{
    auto v = new vector!char;
    auto v1 = new vector!char;

    v.reserve(10);
    writeln("vector reserved with size of: ", v.capacity());

    foreach (c; "Europa")
        v.push_back(c);

    immutable term = '\0';
    v.push_back(term);

    string reconstruct;
    for (int i = 0; i < v.size(); i++)
        reconstruct ~= to!string(v.at(i));

    write("printing vector with writeln: ");
    writeln(reconstruct);

    writeln("vector length is: ", v.size());
    v.resize(5);
    writeln("vector length after resize(5): ", v.size());

    writeln("vector capacity = ", v.capacity());

    write("printing vector with iterator: ");
    vector!(char).iterator it = v.begin();
    for (int i = 0; i < v.size(); it++, i++)
        write(*it);
    writeln(*it);


    writeln("writing second vector with iterator: ");
    auto arr = "567";
    v1.assign(arr.ptr, arr.ptr+3);
    it = v1.begin;
    for (int i = 0; i < v.size(); it++, i++)
        write(*it);
    writeln(*it);

//     const char x = '5';
//     writeln("inserting character into first vector: ");
//     it = v.begin;
//     v.insert(it, x); // FAILURE in C++11 mode: D template argument deduction fails because it tries to match
            // ._cpp.std.__are_same.__are_same!(_Iter, _Container.pointer)).__value, _Container).__type
            //  to
            // _cpp.std.vector.vector!char.vector
}
