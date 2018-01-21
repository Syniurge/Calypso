#pragma once

struct S {
    int* keyhole;
    int val;

    ~S() {
        if (keyhole)
            *keyhole = val;
        keyhole = 0;
    }

    S clone() {
        S r; r.keyhole = keyhole; r.val = val;
        return r;
    }
};

struct S2 {
    int n;

    static unsigned refs;
    static bool hasZeroRefs() { return !refs; }

    S2() { refs++; }
    S2(const S2&) { refs++; }
    ~S2() { refs--; }

    static inline S2 build() { return S2(); }
};

unsigned S2::refs = 0;
