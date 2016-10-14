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

	struct __cpp_member_ptr(T, Cls)
		if (!is(T == function))
	{
		static assert(__traits(isCpp, Cls), "Cls must be C++ class/struct");
		
		ptrdiff_t offset;
	}
	
	struct __cpp_member_ptr(T, Cls)
		if (is(T == function))
	{
		static assert(__traits(isCpp, Cls), "Cls must be C++ class/struct");
		
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
	// The number of extra slots depends on the record, and may be:
    //	 NVOffsetField(0), VBPtrOffsetField(1), VBTableOffsetField(2)
	// If there's no extra slot however, the LLVM type is not a struct type,
	// but the type of the first field.
	struct __cpp_member_ptr(T, Cls)
	{
		static assert(__traits(isCpp, Cls), "Cls must be C++ class/struct");
		
		static if (is(T == function))
			void* ptr;  // FunctionPointerOrVirtualThunk
		else
			int offset; // FieldOffset
			
		enum extraSlots = __traits(getMemberPointerExtraSlots, Cls);
		// enum bool[3] extraSlots = [
			// __traits(getMemberPointerExtraSlots, Cls)[0],
			// __traits(getMemberPointerExtraSlots, Cls)[1],
			// __traits(getMemberPointerExtraSlots, Cls)[2],			
		// ];
			
		static if (extraSlots[0] && is(T == function))
			int nvOffset;           // Non-Virtual Offset Field
		static if (extraSlots[1])
			int vbptrOffsetField;   // VBPtrOffsetField
		static if (extraSlots[2])
			int vbtableOffsetField; // VBTableOffsetField
	}
}
