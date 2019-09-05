// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "conv_operators.hpp");

import (C++) Point;

void main()
{
    Point!int p2i;
    Point!uint p2u = Point!uint(987, 123);

    int mul = cast(int) p2i;
    p2i = cast(Point!int) p2u;

    // Issue #65: https://github.com/Syniurge/Calypso/issues/65
    import (C++) B;
    B b;
    int bx = b.x;
}
