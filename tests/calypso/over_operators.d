// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "over_operators.hpp");

import (C++) Point3D;

void main()
{
    auto z = [ __traits(allMembers, Point3D) ];
    import std.stdio : writeln;
    writeln(z);

    Point3D p;
    Point3D q = Point3D(2.0, -3.0, 5.0);

//     Point3D r = p + q;

    p.x = 1.0;
//     q *= p;
}
