// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::deque example.
 */

module _deque_;

pragma (cppmap, "<deque>");

import std.stdio;
import (C++) std.deque;

void main()
{
    deque!int dq;
    auto dq2 = deque!int(dq);
    auto dq3 = deque!double(10);
    deque!int dq4, dq5;

    writeln("push_back 10 ints onto dq");
    for (int i = 0; i < 10; i++)
    {
        dq.push_back(i);
        dq5.push_back(i);
    }

    // CHECK: dq[2] = 2
    writeln("dq[2] = ", dq[2]);
    // CHECK: dq size = 10
    writeln("dq size = ", dq.size());
    // CHECK: dq = 0 1 2 3 4 5 6 7 8 9
    write("dq = ");
    for (uint i = 0; i < 10; i++)
        write(dq[i], " ");
    writeln();

    // CHECK: dq[0] before pop front = 0
    writeln("dq[0] before pop front = ", dq[0]);
    dq.pop_front();
    // CHECK: dq[0] after pop front = 1
    writeln("dq[0] after pop front = ", dq[0]);

    writeln("dq swap() with dq4");
    dq4.swap(dq);

    // CHECK: dq4 = 1 2 3 4 5 6 7 8 9
    write("dq4 = ");
    for (uint i = 0; i < 9; i++)
        write(dq4[i], " ");
    writeln();

    writeln("fill dq3 with 10 doubles equal to 7/i");
    for (uint i = 0; i < 10; i++)
        dq3[i] = 7F/i;

    // CHECK: dq3 = inf 7 3.5 2.33333 1.75 1.4 1.16667 1 0.875 0.777778
    write("dq3 = ");
    for (uint i = 0; i < 10; i++)
        write(dq3[i], " ");
    writeln();

    writeln("dq.at(3) = 4.5678");
    dq3.at(3) = 4.5678;
    // CHECK: dq3[3] = 4.5678
    writeln("dq3[3] = ", dq3[3]);

    // CHECK: dq3 is empty = No
    writeln("dq3 is empty = " ~ (dq3.empty ? "Yes" : "No"));
    // CHECK: dq3 size = 10
    writeln("dq3 size = ", dq3.size());
    writeln("dq3 max size = ", dq3.max_size());

    writeln("dq3 clear()");
    dq3.clear();

    // CHECK: dq3 is empty = Yes
    writeln("dq3 is empty = " ~ (dq3.empty() ? "Yes" : "No"));
    // CHECK: dq3 size = 0
    writeln("dq3 size = ", dq3.size());

    dq.assign(10,5);

    auto second = new deque!int(4, 100);

    // CHECK: dq5 = 20 20 20 0 1 2 3 4 5 6 7 8 9
    dq5.insert(dq5.begin, 3, 20);
    auto it = dq5.begin();
    write("dq5 = ");
    for (int i = 0; i < dq5.size(); it++, i++)
      write(*it, " ");
    writeln();

    import cpp.std.range : irange;
    foreach (deq; dq3.irange) {}
}
