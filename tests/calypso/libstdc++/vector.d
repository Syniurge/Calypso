// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::vector example.
 */

module _vector_;

modmap (C++) "<vector>";

import std.stdio, std.conv;
import (C++) std.vector;

void main()
{
    vector!char v;
    auto v1 = new vector!char;

    v.reserve(10);
    writeln("vector reserved with size of: ", v.capacity);
    // CHECK: vector reserved with size of: 10

    foreach (c; "Europa")
        v.push_back(c);

    v.push_back('\0');

    string reconstruct;
    for (int i = 0; i < v.size; i++)
        reconstruct ~= v[i];

    write("printing vector (D string): ");
    writeln(reconstruct);
    // CHECK: printing vector (D string): Europa
    write("printing vector with .data(): ");
    writeln(to!string(v.data));
    // CHECK: printing vector with .data(): Europa

    writeln("vector length is: ", v.size);
    // CHECK: vector length is: 7
    v.resize(5);
    writeln("vector length after resize(5): ", v.size);
    // CHECK: vector length after resize(5): 5

    writeln("vector capacity = ", v.capacity);
    // CHECK: vector capacity = 10

    write("printing vector with iterator: ");
    for (auto it = v.begin; it != v.end; it++)
        write(*it);
    writeln();
    // CHECK: printing vector with iterator: Europ

    write("writing second vector with iterator: ");
    auto arr = "567";
    v1.assign(arr.ptr, arr.ptr+3);
    for (auto it = v1.cbegin; it != v1.cend; it++)
        write(*it);
    writeln();
    // CHECK: writing second vector with iterator: 567

    write("inserting character into first vector: ");
    v.insert(v.begin, '#');
    writeln(to!string(v.data));
    // CHECK: inserting character into first vector: #Europ
}
