// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::array example.
 */

module _array_;

modmap (C++) "<array>";
modmap (C++) "<algorithm>";

import std.stdio;
import cpp.std.range;
import (C++) std.array;
import (C++) std._ : sort;

void main()
{
    auto arr = array!(char, 8)(['d', 'i', 'a', 't', 'o', 'm', 'e', 'n']);

    write("before sort: ");
    foreach (c; arr.irange)
        write(c);
    write('\n');
    // CHECK: before sort:{{.*}}diatomen

    sort(arr.begin, arr.end);

    write("after sort:  ");
    foreach (c; arr.irange)
        write(c);
    write('\n');
    // CHECK: after sort:{{.*}}adeimnot
}
