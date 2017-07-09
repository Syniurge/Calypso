// RUN: mkdir %t.cache; \
// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

/**
 * std::bitset example.
 *
 * Build with:
 *   $ ldc2 bitset.d
 */

module _bitset_;

modmap (C++) "<bitset>";

import std.stdio;
import (C++) std.bitset;

void main()
{
    enum : ulong { A=0, B, C, D, E, F, G, H, numColors };
    auto usedColors = new bitset!(numColors);

    usedColors.set(A, true);
    usedColors.set(C, true);

    writeln("usedColors.len = ", numColors);
    write("usedColors = \t ");
    if (usedColors.any())
    {
        for (int i = 0; i < usedColors.size; i++)
            if (usedColors.test(i))
                write('1');
            else
                write('0');
        write('\n');
    }

    writeln("C bit = \t ", usedColors.test(C));      // should be true
    writeln("count = \t ", usedColors.count());      // 2
    writeln("as ulong = \t ", usedColors.to_ulong);  // 5 is correct

    writeln("all = \t\t ", usedColors.all);
    writeln("none = \t\t ", usedColors.none);
    writeln("any = \t\t ", usedColors.any);

    usedColors.flip(C);
    writeln("C flipped = \t ", usedColors.test(C));  // false

    write("b = \t\t ");
    auto a = new bitset!(4u)(0b0110);
    auto b = new bitset!(4u)(0b1101);
    for (int i = 0; i < b.size; i++)
    {
        if (b.test(i))
            write('1');
        else
            write('0');
    }
    write('\n');
    writeln("b as ulong = \t ", b.to_ulong); // '13' is correct
    writeln("b = \t\t", b);

// FAILURE on operators
//    auto d = a&b;

}