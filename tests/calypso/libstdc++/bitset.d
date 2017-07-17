// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

/**
 * std::bitset example.
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
    // CHECK: usedColors.len = 8
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
    // CHECK: usedColors{{.*}}={{.*}}10100000

    writeln("C bit = \t ", usedColors.test(C));
    // CHECK: C bit{{.*}}={{.*}}true
    writeln("count = \t ", usedColors.count());
    // CHECK: count{{.*}}={{.*}}2
    writeln("as ulong = \t ", usedColors.to_ulong);
    // CHECK: as ulong{{.*}}={{.*}}5

    writeln("all = \t\t ", usedColors.all);
    // CHECK: all{{.*}}={{.*}}false
    writeln("none = \t\t ", usedColors.none);
    // CHECK: none{{.*}}={{.*}}false
    writeln("any = \t\t ", usedColors.any);
    // CHECK: any{{.*}}={{.*}}true

    usedColors.flip(C);
    writeln("C flipped = \t ", usedColors.test(C));
    // CHECK: C flipped{{.*}}={{.*}}false

    write("b = \t\t ");
    auto a = bitset!(4u)(0b0110);
    auto b = bitset!(4u)(0b1101);
    for (int i = 0; i < b.size; i++)
    {
        if (b.test(i))
            write('1');
        else
            write('0');
    }
    write('\n');
    // CHECK: b{{.*}}={{.*}}1011
    writeln("b as ulong = \t ", b.to_ulong);
    // CHECK: b as ulong{{.*}}={{.*}}13
    writeln("b =  \t\t", &b);

//    auto d = a & b; // TODO
}
