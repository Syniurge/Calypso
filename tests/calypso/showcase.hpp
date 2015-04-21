#pragma once

namespace test {
    // C++ function
    int testFunc(char c);

    // Struct
    struct testStruct
    {
        float f;
        char c;
        unsigned n;
    };

    // Global variables
    extern double testDoubleVar;
    extern testStruct testVar;
    
    // Simple class with virtual functions
    class testClass
    {
    protected:
        testStruct priv;

    public:
        unsigned n;
        virtual int echo(int a, int b);
        virtual float echo2(float f);
        virtual const wchar_t* testWideString(const wchar_t* defaultNotFound = L"unknown");

        testClass() { n = 1000; }
    };
    
    // Derived class
    class testInherit : public testClass
    {
    public:
        float f;
        char c;

        int echo(int a, int b) override;
        virtual float echo2(float f) override;

        testInherit() { n = 5; }
    };

    // Multiple inheritance
    class anotherClass
    {
    public:
        int number;
        float floating;
        double doubling;

        virtual const char *hello(bool ceres);
    };

    class testMultipleInherit : public anotherClass,
        public testInherit
    {
    public:
        testStruct *pointerToStruct;

        virtual const char *hello(bool pluto) override;
    };

    // Enum
    enum enumTest
    {
        ENUM_FIRSTVAL = 2,
        ENUM_SOMEVAL,
        ENUM_LASTVAL
    };

    // Class template and array
    template<typename T>
     class arrayOfTen
    {
    public:
        T someArray[10];
        T FifthChar() { return someArray[4]; }
    };

    // Partial and explicit template specializations
    template<typename T = unsigned, int N = 1> // primary template
     struct tempWithPartialSpecs
    {
        const char *toChars() { return "Primary template"; }
    };

    template<typename T>
     struct tempWithPartialSpecs<T, 0>
    {
        const char *toChars() { return "Partial spec (N = 0)"; }
    };

    template<typename T>
     struct tempWithPartialSpecs<T, 5>
    {
        const char *toChars() { return "Partial spec (N = 5)"; }
    };

    template<int N>
     struct tempWithPartialSpecs<char, N>
    {
        const char *toChars() { return "Partial spec (T = char)"; }
    };

    template<>
     class tempWithPartialSpecs<bool, 5> // explicit spec
    {
        const char *toChars() { return "Explicit spec (T = bool, N = 5)"; }
    };

    // Function templates
    template<typename T>
     unsigned funcTempSizeOf(T a) { return sizeof(T); }
}
