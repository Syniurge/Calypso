// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "classvalue_dtor.h");

import (C++) B, C, D;

void foo(int* arg) {
    C c = C(arg, 987564);
}

void main()
{
    int n;
    foo(&n);
    assert(n == 987564);

    int n2;
    assert(n2 == 0);
    {
        C c2 = C(&n2, 2314);
    }
    assert(n2 == 2314);

    int n3;
    B* b = new C(&n3, 48625);
    destroy(*b);
    assert(n3 == 48625);

    {
        void foo(D s) {}

        D z;
        z = D.build;
        auto z2 = D();
        D z3 = D();
        D z4 = D.build();
        foo(D.build());
    }
    assert(D.hasZeroRefs);

    version(none) // this doesn't even work with D structs
    {
    import core.memory;
    int n4;
    auto pc2 = new C(&n4, 55255);
    pc2 = null;
    GC.collect();
    assert(n4 == 55255);
    }
}
