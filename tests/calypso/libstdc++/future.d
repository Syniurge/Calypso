// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// R_N: FileCheck %s < %t.out

/**
 * std::future example.
 */

module _future_;

modmap (C++) "<future>";

import std.stdio;
import (C++) std.future, std.future_status;
import (C++) chrono = std.chrono._;
import (C++) std._ : async;

extern(C++) bool is_prime (int x) {
    for (int i = 2; i < x; ++i)
        if (x%i == 0)
            return false;
    return true;
}

void main()
{
    future!bool fut = async(&is_prime, 444444443);

    writeln("checking, please wait");
    auto span = chrono.milliseconds(100);
    while (fut.wait_for(span) == future_status.timeout)
        write('.');

    bool x = fut.get();

    writeln("\n444444443 ", (x?"is":"is not"), " prime.");
}
