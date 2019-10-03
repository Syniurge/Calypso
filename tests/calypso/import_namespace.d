// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// R_N: %t > %t.out
// R_N: FileCheck %s < %t.out

pragma (cppmap, "import_namespace.h");

struct ExhibitA
{
    import (C++) *;

    static void test3164()
    {
        auto myClass = Class9911(5, 2);
        auto myShort = bar4174!short(42);

        NS147.S825 myStruct = { 9 };

        NS147.NS_B598.E2164 myEnumMember = NS147.NS_B598.E2164.EM_XYZ;
    }
}

struct ExhibitB
{
    import (C++) NS147;

    static void test8321()
    {
        S825 someStruct = { 5 };
    }
}

void main()
{
    ExhibitA.test3164();
    ExhibitB.test8321();

    import (C++) * : foo5566;
    foo5566(true);
}
