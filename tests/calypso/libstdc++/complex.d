// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// R_N: FileCheck %s < %t.out

/**
 * std::complex example.
 */

module _complex_;

pragma (cppmap, "<complex>");
pragma (cppmap, "<iostream>");
pragma (cppmap, "<iomanip>");

import std.stdio;
import (C++) std;

void main()
{
    version (CppRuntime_Microsoft) {} else ios_base.Init(); // initialize std::cout

    cout << /+fixed(cout) << +/setprecision(1);

    auto i1 = complex!double(0.0, 1.0);
    auto z1 = i1;
    z1 *= i1;
    cout << "i * i = " << z1 << '\n';

    complex!float z2 = pow(i1, 2);
    cout << "pow(i, 2) = " << z2 << '\n';

    double PI = acos(-1);
//     complex!double z3 = exp(1i * PI);
//     cout << "exp(i * pi) = " << z3 << '\n';

//     complex!float z4 = 1. + 2i, z5 = 1. - 2i;
//     cout << "(1+2i)*(1-2i) = " << z4*z5 << '\n';
}
