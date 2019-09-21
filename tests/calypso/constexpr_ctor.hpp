#pragma once

class A {
public:
    int n;
    float f;

    constexpr A(int n) : n(n), f(9.81) {}

    virtual void makeMeClass() {}
};
