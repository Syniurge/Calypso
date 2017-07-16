#pragma once

struct S000
{
    friend void foo();
    template<typename T> friend void bar(T arg);
};

void foo() {}
template<typename T> void bar(T arg) {}

class C231
{
    friend bool operator==(const C231& __p1, const C231& __p2) {
        return false;
    }
};
