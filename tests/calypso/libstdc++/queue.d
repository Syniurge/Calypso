// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::queue example.
 */

module _queue_;

pragma (cppmap, "<queue>");

import std.stdio;
import (C++) std.queue;

void main()
{
    queue!char q;
    foreach(c; 'a' .. 'z')
        q.push(c);

    writeln(q.size, " characters queued");
    // CHECK: 25 characters queued
    write("q = [");
    while(!q.empty) {
        write(q.front, " ");
        q.pop();
    }
    write("]\n");
    // CHECK: q = [a b c d e f g h i j k l m n o p q r s t u v w x y ]
}
