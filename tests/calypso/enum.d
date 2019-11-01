// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// RUN: FileCheck %s < %t.out

pragma (cppmap, "enum.h");

import std.stdio, std.conv, std.traits;
import (C++) *;

void main()
{
    // CHECK: fullname of AAA_1 = ℂcpp.EnumAAA.EnumAAA.AAA_1
    writeln("fullname of AAA_1 = ", fullyQualifiedName!AAA_1);

    enum E5121 = BBB_5;
    // CHECK: typeof(E5121) = EnumBBB
    writeln("typeof(E5121) = ", typeof(E5121).stringof);

    // CHECK: Anon_2 = 4444
    writeln("Anon_2 = ", Anon_2);
    // CHECK: EnumClass.EC_1 = EC_1
    writeln("EnumClass.EC_1 = ", EnumClass.EC_1);
    static assert(!__traits(compiles, EC_2));

    EnumTypedef tdef = TDEF_1;
    tdef = EnumTypedef.TDEF_2;
    // CHECK: tdef = TDEF_2
    writeln("tdef = ", tdef);

    // CH_CK: test:: members => []
    writeln("test:: members => ", [ __traits(allMembers, test) ]);

    // CHECK: Struct.AnonInStruct_2 = 1
    writeln("Struct.AnonInStruct_2 = ", Struct.AnonInStruct_2);

    // CH_CK: Struct2 members => []
    writeln("Struct2 members => ", [ __traits(allMembers, Struct2) ]);
}
