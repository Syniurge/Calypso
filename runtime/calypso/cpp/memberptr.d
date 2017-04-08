/** This module provides the MFP!(T, alias f) template to generate C++ member function pointers to be passed to C++ code.

    If the C++ code is meant to call obj->*f(...), then T has to be the type of obj (as seen from the C++ code), and f a C++ or D method.
*/
module cpp.memberptr;

import 
    cpp.core, 
    cpp.traits;

alias cpp_member_ptr(T, alias s) = 
    __cpp_member_ptr!(typeof(s), MostDerivedCppClass!T);
    
    
private alias symalias(alias S) = S;

version (CRuntime_Microsoft) {} else
{
    /* Itanium ABI member function pointers
    
        " A pointer to member function is a pair as follows:

          ptr: For a non-virtual function, this field is a simple function pointer. 
        (Under current base Itanium psABI conventions, that is a pointer to a GP/function
        address pair.) For a virtual function, it is 1 plus the virtual table offset (in
        bytes) of the function, represented as a ptrdiff_t. The value zero represents a 
        NULL pointer, independent of the adjustment field value below.
          adj: The required adjustment to this, represented as a ptrdiff_t. " */
          
    template MFP(T, alias f, bool disableThunk = false)
    {
        static assert (is(typeof(f) == function));
    
        alias MFPTy = cpp_member_ptr!(T, f);
        alias T2 = symalias!(__traits(parent, f));

        static if (__traits(getBaseOffset, T, T2) != -1)
            enum ThisAdj = __traits(getBaseOffset, T, T2);
        else
        {
            enum ThisAdj = -__traits(getBaseOffset, T2, T);
            static assert (ThisAdj != -1, T.stringof ~ " not castable to " 
                    ~ MostDerivedCppClass!(__traits(parent, f)).stringof);
        }

        static if (__traits(getCppVirtualIndex, f) != -1)
            enum MFP = MFPTy(1 + __traits(getCppVirtualIndex, f), ThisAdj);
        else
            enum MFP = MFPTy(cast(ptrdiff_t) &f, ThisAdj);
    }
}

version (CRuntime_Microsoft)
{
    import std.traits : Parameters, ReturnType;

    template MFP(T, alias f, bool disableThunk = false)
    {
        static assert (is(typeof(f) == function));
    
        alias MFPTy = cpp_member_ptr!(T, f);
        alias T2 = symalias!(__traits(parent, f));

        static if (__traits(getBaseOffset, T, T2) != -1)
            enum ThisAdj = __traits(getBaseOffset, T, T2);
        else
        {
            enum ThisAdj = -__traits(getBaseOffset, T2, T);
            static assert(ThisAdj != -1, T.stringof ~ " not castable to " 
                    ~ MostDerivedCppClass!(__traits(parent, f)).stringof);
        }

        // if the MFP type doesn't have a NVOffsetField (only there if the C++ class uses multiple inheritance)
        // and T is a DCXX class, then we have to adjust the this pointer in a thunk-like function
        // (ideally this should be a real thunk that minimizes copy ctor calls, but this is the simplest option)
        static if (!disableThunk && ThisAdj != 0 && !MFPTy.extraSlots[0])
        {
            static assert(!__traits(isCpp, T2));
        
            struct DummyT2 {
                extern(C++) ReturnType!f thunk(Parameters!f args) 
                {
                    auto vthis = cast(T2) cast(T*) cast(void*) &this;
                    static if (is(ReturnType!f == void))
                        enum ret = "";
                    else
                        enum ret = "return ";
                    mixin(ret ~ "vthis." ~ __traits(identifier, f) ~ "(args);");
                }
            }
            
            enum firstField = &DummyT2.thunk;
        }
        else static if (!disableThunk && __traits(getCppVirtualIndex, f) != -1)
        {
            static if (__traits(isCpp, T2))
                alias T2ThisTy = T2*;
            else
                alias T2ThisTy = T2;
                    
            static if (MFPTy.extraSlots[0])
                alias ThunkThisTy = T2ThisTy; // there's an offset field available, no need to adjust this in the thunk
            else
                alias ThunkThisTy = T*;
        
            struct DummyT2 {
                extern(C++) ReturnType!f thunk(Parameters!f args) 
                {
                    auto vthis = cast(T2ThisTy) cast(ThunkThisTy) cast(void*) &this;
                    static if (is(ReturnType!f == void))
                        enum ret = "";
                    else
                        enum ret = "return ";
                    mixin(ret ~ "vthis." ~ __traits(identifier, f) ~ "(args);");
                }
            }
            
            enum firstField = &DummyT2.thunk;
        }
        else
            enum firstField = &f;
            
        static if (MFPTy.extraSlots[0])
            enum MFP = MFPTy(firstField, ThisAdj);
        else
            enum MFP = MFPTy(firstField);
    }
}
