module cpp.core;

// std::type_info wrapper for C++ catch clauses
class __cpp_type_info_ptr
{
    void *p;
}

version (CRuntime_Microsoft) {} else
{
	// Itanium ABI member pointer types
	// Template parameters are the type safety enforcers

	// HACK: A struct cannot be used for Itanium data member pointer types, the LLVM type has to match the C++ one,
	//   that is to say ptrdiff_t, so the trick is to use a class, and the *reference itself is the member offset*.
	class __cpp_member_ptr(T, Cls)
	{
		ptrdiff_t dummy;
		static __cpp_member_ptr opCall() { return null; }
	}
	
	struct __cpp_member_funcptr(T, Cls)
		if (is(T == function))
	{
		ptrdiff_t ptr;
		ptrdiff_t adj;
	}

	extern(C) void _d_cpp_init_eh(); // Avoid importing cpp.eh.gnu
	shared static this()
	{
		_d_cpp_init_eh();
	}
} 

version (CRuntime_Microsoft)
{
	// Microsoft ABI member pointer types
	struct __cpp_member_ptr(T, Cls, uint nslots)
	{
		int[nslots] slots;
	}
	
	struct __cpp_member_funcptr(T, Cls) // FIXME
		if (is(T == function))
	{
		ptrdiff_t ptr;
		ptrdiff_t adj;
	}
}
