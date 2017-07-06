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

