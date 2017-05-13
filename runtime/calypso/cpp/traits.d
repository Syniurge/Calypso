module cpp.traits;

template MostDerivedCppClass(T)
    if (is(T == class))
{
    import std.traits : BaseTypeTuple;

    static if (__traits(isCpp, T))
        alias MostDerivedCppClass = T;
    else {
        static assert(BaseTypeTuple!T.length, T.stringof ~ " is not a C++ class or D class inheriting from C++");
        alias MostDerivedCppClass = MostDerivedCppClass!(BaseTypeTuple!T[0]);
    }
}
