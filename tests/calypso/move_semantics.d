// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "move_semantics.h";

import (C++) A;
import (C++) std._ : move;

void foo(A arg) {
}

void main() {
    A.assertMoveCtor = true;
    auto a = A(10);
    foo(a);
    A.assertMoveCtor = false;
    A.assertMainCtor = A.assertCopyCtor = A.assertDtor = true;
    A.bar(move(a));
    static assert(!__traits(compiles, A.bar(a)));
    A.bar(A.barZ(move(a)));
    A.assertDtor = false;
}
