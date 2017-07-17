// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::list example.
 */

module _list_;

modmap (C++) "<list>";

import std.stdio;
import (C++) std.list;

extern(C++) bool single_digit (int value) { return value < 10; }
extern(C++) bool is_odd (int value) { return (value % 2) == 1; }

void main()
{
    list!int l;
    auto l1 = list!int(4, 100);
    auto l2 = new list!int(l);
    list!int l3, l4;

    immutable int x = 11;
    immutable int y = 4;
    immutable int z = 3;
    l.push_back(x);
    l.push_back(y);
    l.push_back(z);
    l.push_back(4);
    l.push_back(3);
    writeln("List front is ", l.front);
    // CHECK: List front is 11

    l.unique();
    l.sort();
    writeln("List front after sort is ", l.front());
    // CHECK: List front after sort is 3

    writeln("\nList:");
    auto k = l.begin;
    for(; k != l.end; k++)
      write(*k, " ");
    // CHECK: 3 3 4 4 11

    writeln("\n\nList2 after swapping in List:");
    l2.swap(l);
    k = l2.begin;
    for (int i=0; i < l2.size ; k++, i++)
      write(*k, " ");
    // CHECK: List2 after swapping in List:
    // CHECK-NEXT: 3 3 4 4 11

    l3.assign(l2.begin, l2.end);

    writeln("\n\nList4 after assign from array:");
    auto arr = [7, 64, 9, 22, 4];
    l4.assign(arr.ptr, arr.ptr+5);
    k = l4.begin;
    for (int i=0; i < l4.size ; k++, i++)
      write(*k, " ");
    // CHECK: List4 after assign from array:
    // CHECK-NEXT: 7 64 9 22 4

    writeln("\n\nList4 after single_digits removed:");
    l4.remove_if(&single_digit);
    k = l4.begin;
    for (int i=0; i < l4.size ; k++, i++)
      write(*k, " ");
    writeln();
    // CHECK: List4 after single_digits removed:
    // CHECK-NEXT: 64 22

    l4.remove_if(&is_odd);
}
