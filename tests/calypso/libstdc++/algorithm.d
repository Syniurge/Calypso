// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::algorithm example.
 */

pragma (cppmap, "<algorithm>");
pragma (cppmap, "<vector>");

import std.stdio;
import cpp.std.range;
import (C++) std._ : for_each;
import (C++) std.vector;

void main()
{
    vector!int nums;
//     nums = [3, 4, 2, 8, 15, 267]; // TODO?
    foreach (n; [3, 4, 2, 8, 15, 267])
        nums.push_back(n);

    write("before:");
    foreach (n; nums.irange)
        write(' ', n);
    write('\n');
    // CHECK: before:{{.*}}3 4 2 8 15 267

    extern(C++) void Inc(ref scope int n) { n++; }
    for_each(nums.begin, nums.end, &Inc);

    write("after: ");
    foreach (n; nums.irange)
        write(' ', n);
    write('\n');
    // CHECK: after:{{.*}}4 5 3 9 16 268
}
