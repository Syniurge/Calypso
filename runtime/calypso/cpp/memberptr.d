module cpp.memberptr;

import 
	cpp.core, 
	cpp.traits;

alias cpp_member_ptr(T, alias s) = 
	__cpp_member_ptr!(typeof(s), MostDerivedCppClass!T);

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
		  
	template MFP(T, alias f)
	{
		static assert (is(typeof(f) == function));
	
		alias MFPTy = cpp_member_ptr!(T, f);
		alias T2 = MostDerivedCppClass!(__traits(parent, f));

		static if (__traits(getBaseOffset, T, T2) != -1)
			enum ThisAdj = -__traits(getBaseOffset, T, T2);
		else
		{
			enum ThisAdj = __traits(getBaseOffset, T2, T);
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

	template MFP(T, alias f)
	{
		static assert (is(typeof(f) == function));
	
		alias MFPTy = cpp_member_ptr!(T, f);
		alias T2 = MostDerivedCppClass!(__traits(parent, f));

		static if (__traits(getBaseOffset, T, T2) != -1)
			enum ThisAdj = -__traits(getBaseOffset, T, T2);
		else
		{
			enum ThisAdj = __traits(getBaseOffset, T2, T);
			static assert(ThisAdj != -1, T.stringof ~ " not castable to " 
					~ MostDerivedCppClass!(__traits(parent, f)).stringof);
		}

		static if (__traits(getCppVirtualIndex, f) != -1)
		{
			static if (MFPTy.extraSlots[0])
				alias ThunkThisTy = T2; // there's an offset field available, no need to adjust this in the thunk
			else
				alias ThunkThisTy = T;

			extern(C++) ReturnType!f thunk(ThunkThisTy* vthis, Parameters!f args) 
			{
				static if (is(ReturnType!f == void))
					f(vthis, args);
				else
					return f(vthis, args);
			}
			
			enum firstField = &thunk;
		}
		else
			enum firstField = &f;
			
		static if (MFPTy.extraSlots[0])
			enum MFP = MFPTy(firstField, ThisAdj);
		else
			enum MFP = MFPTy(firstField);
	}
}
