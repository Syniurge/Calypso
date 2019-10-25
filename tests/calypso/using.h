#pragma once

#include <assert.h>

class BaseClass1
{
public:
    void overloaded(int i) {}
};

class InheritingClass1 : public BaseClass1
{
public:
    using BaseClass1::overloaded;
    void overloaded(const char* s) {}
};

class BaseClass2
{
public:
    void overloaded(int i) { assert(false); }
};

class InheritingClass2 : public BaseClass2
{
public:
    using BaseClass2::overloaded; // doesn't result in any import (no UsingShadowDecl)

    void overloaded(int i) {}
    void overloaded(const char* s) {}
};

class BaseClass3
{
public:
    void overloaded(int i) {}
    void overloaded(const char* s) {}
};

class InheritingClass3 : public BaseClass3
{
public:
    using BaseClass3::overloaded;

    void overloaded(float f) {}
};

class BaseClass4
{
public:
    void overloaded(int i) {}
    template<typename T> void overloaded(T** a) { assert(false); }
};

class InheritingClass4 : public BaseClass4
{
public:
    using BaseClass4::overloaded;

    void overloaded(float f) {}
    template<typename T> void overloaded(T** a) {}
};

class BaseClass5
{
public:
    void overloaded(int i) {}
    template<typename T> void overloaded(T** a) {}
};

class InheritingClass5 : public BaseClass5
{
public:
    using BaseClass5::overloaded;

    void overloaded(float f) {}
};

namespace Namespace5
{
    static int var = 1000;
    class SomeClass {};
    template<typename T> struct TemplatedStruct {};
}

namespace Namespace6
{
    using Namespace5::var;
    using Namespace5::SomeClass;
    using Namespace5::TemplatedStruct;
}
