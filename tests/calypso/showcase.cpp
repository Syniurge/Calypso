#include "showcase.hpp"

namespace test
{

double testDoubleVar = 99.99;
testStruct testVar = { 42.24, 'c', 555 };

int testFunc(char c)
{
    if (c == 'a')
        return 2;
    else if (c == 'b')
        return 5;
    else
        return 8;
}

int testClass::echo(int a, int b)
{
    return -1;
}

float testClass::echo2(float f)
{
    return -1.0;
}

const wchar_t* testClass::testWideString(const wchar_t* defaultNotFound)
{
    return defaultNotFound;
}

int testInherit::echo(int a, int b)
{
    return 42 * (a-b) * n;
}

float testInherit::echo2(float f)
{
    return 0.5 * f;
}

const char *anotherClass::hello(bool ceres)
{
    return ceres ? "Ceres is true" : "Ceres is false";
}

const char *testMultipleInherit::hello(bool pluto)
{
    return pluto ? "Pluto => true" : "Pluto => false";
}

}
