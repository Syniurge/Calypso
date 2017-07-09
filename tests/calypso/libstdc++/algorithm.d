/**
 * std::algorithm example.
 *
 * Build with:
 *   $ ldc2 algorithm.d
 */

modmap (C++) "<algorithm>";
modmap (C++) "<vector>";

import std.stdio;
import cpp.std.range;
import (C++) std._;
import (C++) std.vector;

struct Sum
{
    int sum = 0;
    void opCall(int n) { sum += n; }
}


void main()
{
    vector!int nums;
//     nums = [3, 4, 2, 8, 15, 267]; // TODO: std::initializer_list
    foreach (n; [3, 4, 2, 8, 15, 267])
        nums.push_back(n);

    write("before:");
    foreach (n; nums.irange)
        write(' ', n);
    write('\n');

    extern(C++) void Inc(ref scope int n) { n++; }
    for_each(nums.begin, nums.end, &Inc); // TODO: function literals?

    // calls Sum::operator() for each number
    // FAILURE: Sum isn't a C++ struct
//     Sum s = for_each(nums.begin(), nums.end(), *new Sum);

    write("after: ");
    foreach (n; nums.irange)
        write(' ', n);
    write('\n');
//     write("sum: ", s.sum, '\n');
}