#pragma once

class A {
    int n;
    float f;
public:
    constexpr A(int n) : n(n), f(9.81) {}

    virtual void makeMeClass() {}
};
