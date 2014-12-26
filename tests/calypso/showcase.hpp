#pragma once

// #include "OGRE/Ogre.h"

namespace test {
    int testFunc(char c);

    struct testStruct
    {
        float f;
        char c;
        unsigned n;
    };

    extern double testDoubleVar;
    extern testStruct testVar;
    
    class testClass
    {
    protected:
        testStruct priv;

    public:
        unsigned n;
        virtual int echo(int a, int b);
        virtual float echo2(float f);
    };
    
    class testInherit : public testClass
    {
    public:
        float f;
        char c;

        int echo(int a, int b) override;
        virtual float echo2(float f) override;
    };

    class anotherClass
    {
    public:
        int number;
        float floating;
        double doubling;

        virtual bool hello(bool who);
    };

    class testMultipleInherit : public anotherClass,
        public testInherit
    {
    public:
        testStruct *pointerToStruct;

        virtual bool hello(bool who) override;
    };

    enum enumTest
    {
        ENUM_FIRSTVAL = 2,
        ENUM_SOMEVAL,
        ENUM_LASTVAL
    };

    template<typename _CharT>
    struct char_arrayten
    {
        _CharT someCharArray[10];
        _CharT FifthChar() { return someCharArray[4]; }
    };

    int implicitSpecPlease(char_arrayten<char> &o);
}
