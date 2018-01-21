#pragma once

class B {
public:
    char xyz;
    virtual ~B() {
        xyz = 'a';
    }
};

class C : public B {
    int* keyhole;
    int val;
    C(int* keyhole, int val) : keyhole(keyhole), val(val) {}
    ~C() {
        if (keyhole)
            *keyhole = val;
        keyhole = 0;
    }
};

class D : public B {
    static unsigned refs;
    static bool hasZeroRefs() { return !refs; }

    D() { refs++; }
    D(const D&) { refs++; }
    ~D() { refs--; }

    static inline D build() { return D(); }
};

unsigned D::refs = 0;
