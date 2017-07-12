#pragma once

class Cls
{
public:
    unsigned n;
    virtual int foo(int a, int b) { return -1; }
    virtual float foo2(float f) { return -1.0; }

    Cls() { n = 1000; }
};

class Inh : public Cls
{
public:
    float f;
    char c;

    int foo(int a, int b) override final { return 2; }
    float foo2(float f) override { return 2.0; }

    Inh() { n = 5; }
};

class ClsB
{
public:
    int number;
    float floating;
    double doubling;

    virtual const char *bar(bool arg) { return arg ? "arg is true" : "arg is false"; }
};

class MulInh : public ClsB, public Inh
{
public:
    unsigned *pointerToUint;

    float foo2(float f) override { return 4.0; }
    const char *bar(bool arg) override { return "arg doesn't matter"; }
};
