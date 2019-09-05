// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "over_operators.hpp");

import (C++) Point3D;
import std.stdio : writeln;

void main()
{
    Point3D p;
    Point3D q = Point3D(2.0, -3.0, 5.0);

    p.x = 1.0;
    q *= p + q;

    writeln("q result = ", q.x, ", ", q.y, ", ", q.z);
    // CHECK: q result = 6, 9, 25
}
