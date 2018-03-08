// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// R_N: %t > %t.out
// R_N: FileCheck %s < %t.out

pragma (cppmap, "friend_decl.h");

void main()
{
    {
        import (C++) _;
        foo();
        bar(9);
    }

    {
        import (C++) C231;
        C231 a, b;
        bool result = opEquals(a, b);
    }
}
