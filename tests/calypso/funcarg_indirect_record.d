// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "funcarg_indirect_record.h";
import (C++) B;

void fun1(B x) {
    x.test;
}

void main() {
    auto a = B(10);
    auto a2 = a;
    a2 = a;
    fun1(a);
}
