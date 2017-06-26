#pragma once

class B {};

class C : B {
    int* keyhole;
    int val;
    C(int* keyhole, int val) : keyhole(keyhole), val(val) {}
    virtual ~C() { *keyhole = val; }
};

