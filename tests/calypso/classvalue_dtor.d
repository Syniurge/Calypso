modmap (C++) "classvalue_dtor.h";

import (C++) C;

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
    auto pc = new C(&n3, 48625);
    destroy(*pc);
    assert(n3 == 48625);
    pc.keyhole = &n3; // destroy() zerosets *pc but the dtor gets called a second time by the GC so .keyhole needs to be valid
}
