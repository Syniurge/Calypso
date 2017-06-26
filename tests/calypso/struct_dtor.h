#pragma once

struct S {
    int* keyhole;
    int val;
    ~S() { *keyhole = val; }
};

