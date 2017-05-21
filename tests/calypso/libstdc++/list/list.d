/**
 * std::list example.
 *
 * Build with:
 *   $ ldc2 list.d
 */

module _list_;

modmap (C++) "<list>";

import std.stdio, std.conv, std.string;
import (C++) std.list;

extern(C++) bool single_digit (int value) { return (value<10); }
extern(C++) bool is_odd (int value) { return (value % 2) == 1; }

void main()
{
    auto l = new list!int;
    auto l1 = new list!int(4, 100);
    auto l2 = new list!int(*l);
    auto l3 = new list!int;
    auto l4 = new list!int;

    immutable int x = 11;
    immutable int y = 4;
    immutable int z = 3;
    l.push_back(x);
    l.push_back(y);
    l.push_back(z);
    l.push_back(y);
    l.push_back(x);
    writeln("List front is ", l.front());

    l.unique();
    l.sort();
    writeln("List front after sort is ", l.front());

    writeln("\nList:");
    auto k = l.begin();
    for(; k != l.end(); k++)
      writeln(*k);

    writeln("\nList2 after swapping in List:");
    l2.swap(*l);
    k = l2.begin;
    for (int i=0; i < l2.size() ; k++, i++)
      writeln(*k);

    l3.assign(l2.begin(), l2.end());

    writeln("\nList4 after assign from array:");
    auto arr = [7, 64, 9, 22, 4];
    l4.assign(arr.ptr, arr.ptr+5);
    k = l4.begin;
    for (int i=0; i < l4.size() ; k++, i++)
      writeln(*k);

    writeln("\nList4 after single_digits removed:");
    l4.remove_if(&single_digit);
    k = l4.begin;
    for (int i=0; i < l4.size() ; k++, i++)
      writeln(*k);

    l4.remove_if(&is_odd);
}
