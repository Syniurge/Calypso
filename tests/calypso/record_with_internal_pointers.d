// Checks that no extra copy takes place, breaking the internal pointer B.y
// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

pragma (cppmap, "record_with_internal_pointers.h");
import (C++) B, _;

void fun1(B x) {
    x.test;
}

void main() {
    auto a = B(10);
    auto a2 = a;
    a2 = a;
    fun1(a);

    foo().test();
}
