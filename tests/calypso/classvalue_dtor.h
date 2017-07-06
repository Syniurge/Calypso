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
