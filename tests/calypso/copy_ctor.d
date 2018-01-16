// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "copy_ctor.h";

import (C++) StaticCount, StaticCountDerived;

void fun(StaticCount a) {}

void main()
{
    StaticCount u1, u2;
    assert(StaticCount.copyctorcalls == 0);
    assert(StaticCount.dtorcalls == 0);

    u1 = u2;
    assert(StaticCount.copyctorcalls == 1);
    assert(StaticCount.dtorcalls == 1);

    u1.assignIndirect(u2);
    assert(StaticCount.copyctorcalls == 3);
    assert(StaticCount.dtorcalls == 3);

    u1.idleByRef(u2);
    assert(StaticCount.copyctorcalls == 3);
    assert(StaticCount.dtorcalls == 3);

    u1 = u2.clone();
    assert(StaticCount.copyctorcalls == 4);
    assert(StaticCount.dtorcalls == 4);

    StaticCount u3 = u1;
    assert(StaticCount.copyctorcalls == 5);
    assert(StaticCount.dtorcalls == 4);

    fun(u1);
    assert(StaticCount.copyctorcalls == 6);
    assert(StaticCount.dtorcalls == 5);

    assert(StaticCount.refs == 3);
    assert(StaticCount.assigns == 3);

    StaticCount.copyctorcalls = 0;
    StaticCount.dtorcalls = 0;
    StaticCount.refs = 0;
    StaticCount.assigns = 0;

    StaticCountDerived v1, v2;
    assert(StaticCount.copyctorcalls == 0);
    assert(StaticCount.dtorcalls == 0);

    v1 = v2;
    assert(StaticCount.copyctorcalls == 1);
    assert(StaticCount.dtorcalls == 1);

    v1.assignIndirect(v2);
    assert(StaticCount.copyctorcalls == 3);
    assert(StaticCount.dtorcalls == 3);

    v1.idleByRef(v2);
    assert(StaticCount.copyctorcalls == 3);
    assert(StaticCount.dtorcalls == 3);

    v1 = v2.clone();
    assert(StaticCount.copyctorcalls == 4);
    assert(StaticCount.dtorcalls == 4);

    StaticCountDerived v3 = v1;
    assert(StaticCount.copyctorcalls == 5);
    assert(StaticCount.dtorcalls == 4);

    assert(StaticCount.refs == 3);
    assert(StaticCount.assigns == 3);
}
