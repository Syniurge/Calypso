modmap (C++) "dynamic_cast.h";

import (C++) B, C, SB, SC;

void main()
{
    char destChar;

    // class -> class upcasts
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

    // struct -> class upcasts
    SB* sb1 = new SC;
    SC* sc1 = cast(SC*) sb1;
    assert(sc1 != null);

    SB* sb2 = new SB;
    SC* sc2 = cast(SC*) sb2;
    assert(sc2 == null);
}
