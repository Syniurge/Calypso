// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "struct_dtor.h");

import (C++) S, S2;

void foo(int* arg) {
    S s = { arg, 987564 };
}

void bar(int* arg) {
    new S(arg, 55255);
}

void main()
{
    int n;
    foo(&n);
    assert(n == 987564);

    int n2;
    assert(n2 == 0);
    {
        S s2 = { &n2, 2314 };
    }
    assert(n2 == 2314);

    int n3;
    auto ps = new S(&n3, 48625);
    destroy(*ps);
    assert(n3 == 48625);

    {
        void foo(S2 s) {}

        S2 z;
        z = S2.build;
        auto z2 = S2();
        S2 z3 = S2();
        S2 z4 = S2.build();
        foo(S2.build());
    }
    assert(S2.hasZeroRefs);

    version(none) // this doesn't even work with D structs
    {
    import core.memory;
    int n4;
    bar(&n4);
    GC.collect();
    assert(n4 == 55255);
    }
}
