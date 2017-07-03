#pragma once

struct StaticCount
{
    static int refs;
    static int copyctorcalls;
    static int dtorcalls;
    static int assigns;

    StaticCount() {
        refs++;
    }

    StaticCount(const StaticCount&) {
        copyctorcalls++;
        refs++;
    }

    ~StaticCount()
    {
        refs--;
        dtorcalls++;
    }

    StaticCount& operator=(StaticCount u) {
        assigns++;
        return *this;
    }

    void assignIndirect(StaticCount u) {
        *this = u;
    }

    void idleByRef(StaticCount& u) {
    }

    StaticCount clone() {
        return StaticCount(*this);
    }
};

int StaticCount::refs = 0;
int StaticCount::copyctorcalls = 0;
int StaticCount::dtorcalls = 0;
int StaticCount::assigns = 0;

class StaticCountDerived : public StaticCount
{
    StaticCountDerived() : StaticCount() {}
    StaticCountDerived(const StaticCountDerived &o)  : StaticCount(o) {}

    StaticCountDerived& operator=(StaticCountDerived u) {
        assigns++;
        return *this;
    }

    void idleByRef(StaticCountDerived& u) {
    }

    StaticCountDerived clone() {
        return StaticCountDerived(*this);
    }
};
