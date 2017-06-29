#pragma once

class B {
public:
    char* abc;
    virtual void foo() {
        *abc = 'z';
    }
};

class C : public B {
    void foo() {
        *abc = 'x';
    }
};

struct SB {
};

class SC : public SB {
};
