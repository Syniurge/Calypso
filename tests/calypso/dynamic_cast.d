// RUN: mkdir %t.cache; \
// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "dynamic_cast.h";

import (C++) B, C;

void main()
{
    char destChar;

    B* b1 = new C;
    b1.abc = &destChar;
    b1.foo();
    assert(destChar == 'x');
    C* c1 = cast(C*) b1;
    assert(c1 != null);

    B* b2 = new B;
    b2.abc = &destChar;
    b2.foo();
    assert(destChar == 'z');
    C* c2 = cast(C*) b2;
    assert(c2 == null);
}
