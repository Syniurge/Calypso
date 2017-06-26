modmap (C++) "struct_dtor.h";

import (C++) S;

void foo(int* arg) {
    S s = { arg, 987564 };
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
    ps.keyhole = &n3; // destroy() zerosets *ps but the dtor gets called a second time by the GC so .keyhole needs to be valid
}
