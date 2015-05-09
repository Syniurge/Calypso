/**
 * std::deque example.
 *
 * Build with:
 *   $ ldc2 -L-lstdc++ deque.d
 */

modmap (C++) "<deque>";

import std.stdio;
import (C++) std.deque;

void main()
{
    auto dq = new deque!int;
    auto dq2 = new deque!(int)(dq);
    auto dq3 = new deque!(double)(10);
    auto dq4 = new deque!(int);
    auto dq5 = new deque!(int);

    // Fill deque with 10 ints
    writeln("push_back 10 ints onto dq");
    for (int i = 0; i < 10; i++)
    {
        dq.push_back(i);
        dq5.push_back(i);
    }

    // Print out
    writeln("dq[2] = ", dq[2]);
    writeln("dq size = ", dq.size());
    write("dq = ");
    for (uint i = 0; i < 10; i++)
    {
        write(dq[i]);
        write(" ");
    }
    writeln();

    writeln("dq[0] before pop front = ", dq[0]);
    dq.pop_front();
    writeln("dq[0] after pop front = ", dq[0]);

    // Swap a couple deques and print out the swap
    writeln("dq swap() with dq4");
    dq4.swap(dq);

    write("dq4 = ");
    for (uint i = 0; i < 10; i++)
    {
        write(dq4[i]);
        write(" ");
    }
    writeln();

    // Fill deque3 with doubles and print out
    writeln("fill dq3 with 10 doubles equal to 7/i");
    for (uint i = 0; i < 10; i++)
        dq3[i] = 7F/i;

    write("dq3 = ");
    for (uint i = 0; i < 10; i++)
    {
        write(dq3[i]);
        write(" ");
    }
    writeln();

    // Test the .at() capability
    writeln("dq.at(3) = 4.5678");
    dq3.at(3) = 4.5678;
    writeln("dq3[3] = ", dq[3]);

    // Test a few simple deque functions    
    writeln("dq3 is empty = " ~ (dq3.empty() ? "Yes" : "No"));
    writeln("dq3 size = ", dq3.size());
    writeln("dq3 max size = ", dq3.max_size());

    writeln("dq3 clear()");
    dq3.clear();

    writeln("dq3 is empty = " ~ (dq3.empty() ? "Yes" : "No"));
    writeln("dq3 size = ", dq3.size());

    dq.assign(10,5);
    auto it = new deque!(int).iterator;

    // FAILURES

    // Assertion `int_regs + sse_regs <= 2' failed !?!?!?
    //it = dq5.begin();
    // Idiomatic C++ iterator usage doesn't work yet. See list.d also
    //for (int i=0; i < dq.size(); it++, i++)
      //writeln(*it);

    // 'no property popFront' here
    //foreach(deq; dq3) {}

    //auto second = new deque!(int)(4, 100);

    // Doesn't seem to be picking up all template decls
    //immutable int xx = 9;
    //immutable float yy = 8.1;
    //auto dq5 = new deque!(xx, yy);

    // deque iterators need to work for this to work
    // dq.insert(it, 2, 20);
}