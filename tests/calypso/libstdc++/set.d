// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::set example.
 */

module _set_;

modmap (C++) "<set>";

import std.stdio;
import (C++) std.set;
import (C++) std.multiset;

void main()
{
    set!char s;
    auto ms1 = new multiset!int;
    auto ms2 = new multiset!int;

    int[] ints = [1, 9, 55, 77, 99, 33];
    auto ms3 = multiset!int(ints.ptr, ints.ptr+3);

    ms1.insert(ms1.begin, 7);

    for (int i = 0; i < 5; i++)
        ms1.insert(i);

    write("multiset1 = ");
    for (auto it = ms1.begin; it != ms1.end; it++)
        write(*it, " ");
    writeln();
    // CHECK: multiset1 = 0 1 2 3 4 7

    ms2.insert(ints.ptr, ints.ptr+3);

    write("multiset2 after ranged insert = ");
    for (auto it = ms2.begin; it != ms2.end; it++)
        write(*it, " ");
    writeln();
    // CHECK: multiset2 after ranged insert = 1 9 55

    auto kcomp = ms1.key_comp;
    auto rit = ms1.rbegin;
    auto highest = *rit;

    do {
        writeln("multiset1 using compare and reverse iterator = ", *rit);
    } while ( kcomp(*rit++, highest) );
    // CHECK: multiset1 using compare and reverse iterator = 7
}
