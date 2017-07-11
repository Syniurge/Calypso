// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "struct_dtor.h";

import (C++) S;

void foo(int* arg) {
    S s = { arg, 987564 };
}

void bar(int* arg)
{
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

    version(none) // this doesn't even work with D structs
    {
    import core.memory;
    int n4;
    bar(&n4);
    GC.collect();
    assert(n4 == 55255);
    }
}
