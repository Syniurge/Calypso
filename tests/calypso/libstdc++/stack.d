// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::stack example.
 */

module _stack_;

pragma (cppmap, "<stack>");
pragma (cppmap, "<vector>");
pragma (cppmap, "<deque>");

import std.stdio;
import (C++) std.stack;
import (C++) std.vector;
import (C++) std.deque;

void main()
{
    deque!int dq;
    for (int i = 0; i < 10; i++)
      dq.push_back(i);

    auto v = vector!int(2, 200);

    stack!char st1;
    auto dq2 = stack!int(dq);
    auto st2 = new stack!(int, vector!int);

    writeln("empty stack size = ", st1.size);
    // CHECK: empty stack size = 0
    writeln("10 element deque stack size = ", dq2.size);
    // CHECK: 10 element deque stack size = 10

    while(!dq2.empty) {
        writeln("popping the top of a stack of 10 elements from ordered deque = ", dq2.top);
        dq2.pop();
    }
    // CHECK: popping the top of a stack of 10 elements from ordered deque = 9
    // CHECK: popping the top of a stack of 10 elements from ordered deque = 5
    // CHECK: popping the top of a stack of 10 elements from ordered deque = 1
}
