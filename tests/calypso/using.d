// RUN: %ldc -cpp-args -std=c++14 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out
// R_N: FileCheck %s < %t.out

pragma(cppmap, "using.h");

import(C++) *;

void main()
{
    auto inheritingClass1 = new InheritingClass1;
    inheritingClass1.overloaded(1);

    auto inheritingClass2 = new InheritingClass2;
    inheritingClass2.overloaded(2);

    auto inheritingClass3 = new InheritingClass3;
    inheritingClass3.overloaded("str");

    auto inheritingClass4 = new InheritingClass4;
    inheritingClass4.overloaded(cast(void**)null);

    auto inheritingClass5 = new InheritingClass5;
    inheritingClass5.overloaded(cast(void**)null);

    auto a = Namespace6.var;
    Namespace6.SomeClass b;
    Namespace6.TemplatedStruct!int c;
}
