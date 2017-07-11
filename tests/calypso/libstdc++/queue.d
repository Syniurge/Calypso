// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

/**
 * std::queue example.
 *
 * Build with:
 *   $ ldc2 queue.d
 */

module _queue_;

modmap (C++) "<queue>";

import std.stdio, std.conv, std.string;
import (C++) std.queue;

void main()
{
    auto q = new queue!(char);
    writeln("queue compiles");
}
