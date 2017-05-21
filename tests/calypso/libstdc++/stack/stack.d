/**
 * std::stack example.
 *
 * Build with:
 *   $ ldc2 stack.d
 */

module _stack_;

modmap (C++) "<stack>";
modmap (C++) "<vector>";
modmap (C++) "<deque>";

import std.stdio;
import (C++) std.stack;
import (C++) std.vector;
import (C++) std.deque;

void main()
{
    auto dq = new deque!(int);
    for (int i = 0;i<10; i++)
      dq.push_back(i);

    auto v = new vector!(int)(2,200);

    auto st1 = new stack!char;
    auto dq2 = new stack!(int)(*dq);
    auto st2 = new stack!(int, vector!int);

    writeln("empty stack size = ", st1.size());
    writeln("10 element deque stack size = ", dq2.size());

    for (int i = 0;i<10; i++)
    {
        writeln("popping the top of a stack of 10 elements from ordered deque = ", dq2.top());
        dq2.pop();
    }
}