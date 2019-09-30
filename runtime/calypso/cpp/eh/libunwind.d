/**
 * This module implements the C++ foreign exception handlers for Calypso.
 *
 * libstdc++ and libc++ differ slightly ABI-speaking, enough to warrant
 * two separate handlers.
 *
 * Copyright: Authors 2015-2018
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Elie Morisse
 */

module cpp.eh.libunwind;

version (CRuntime_Microsoft) {} else
{

import rt.dwarfeh;
import rt.unwind;
import cpp.core;

pragma (cppmap, "unwind-cxx-d.h");

import (C++) _Unwind_Exception : _cpp__Unwind_Exception = _Unwind_Exception;

// There are slight vtable and struct layout differences in libc++ compared to libstdc++:
//     - std::type_info::can_catch has one less parameter
//     - __cxa_exception/__cxa_dependent_exception have an extra referenceCount/primaryException field
//     - caught pointer objects aren't expected to be deferenced i.e passed by value

struct ABI_libcxx
{
    static bool is_exception_class(ulong c)
    {
        import (C++) __cxxabiv1.libcxx._;
        return (c & get_vendor_and_language) == (kOurExceptionClass & get_vendor_and_language);
    }

    import (C++) __cxxabiv1.libcxx.type_info : type_info;
    import (C++) __cxxabiv1.libcxx.__cxa_exception : __cxa_exception;
    import (C++) __cxxabiv1.libcxx.__cxa_dependent_exception : __cxa_dependent_exception;
    import (C++) __cxxabiv1.libcxx._ : cxa_exception_from_exception_unwind_exception,
                                       get_thrown_object_ptr;
}

struct ABI_libstdcxx
{
    alias is_exception_class = __is_gxx_exception_class;

    import (C++) __cxxabiv1.libstdcxx.type_info : type_info;
    import (C++) __cxxabiv1.libstdcxx.__cxa_exception : __cxa_exception;
    import (C++) __cxxabiv1.libstdcxx.__cxa_dependent_exception : __cxa_dependent_exception;
    import (C++) __cxxabiv1.libstdcxx._ :
                        __is_gxx_exception_class,
                        cxa_exception_from_exception_unwind_exception = __get_exception_header_from_ue,
                        get_thrown_object_ptr = __get_object_from_ue;
}

class CppHandler : ForeignHandler
{
    private bool doCatchImpl(ABI)(void* entry, _Unwind_Exception* exceptionObject)
    {
        static ABI.type_info *getCatchTypeInfo(void* entry)
        {
            auto a = cast(__calypso_type_info_ptr)cast(Object)entry;
            return a ? cast(ABI.type_info*)a.p : null;
        }

        auto catchTypeInfo = getCatchTypeInfo(entry);
        if (!catchTypeInfo)
            return false; // not a catch (C++) clause

        ABI.__cxa_exception *_cpp_exception =
                ABI.cxa_exception_from_exception_unwind_exception(cast(_cpp__Unwind_Exception*) exceptionObject);
        void *__thr_obj = ABI.get_thrown_object_ptr(&_cpp_exception.unwindHeader);

        // libstdc++ only: for pointer types it's the actual pointer that needs to get adjusted, not the pointer to pointer that is the exception object.
        // This also has the effect of passing pointer types "by value" through the __cxa_begin_catch return value.
        static if (is(ABI == ABI_libstdcxx))
            if (_cpp_exception.exceptionType.__is_pointer_p())
                __thr_obj = *cast(void **) __thr_obj;

        if (catchTypeInfo.can_catch(_cpp_exception.exceptionType, &__thr_obj))
        {
            _cpp_exception.adjustedPtr = __thr_obj; // NOTE: __cxa_begin_catch returns adjustedPtr, which in C++ EH is set by the personality routine if the search phase is successful
            return true;
        }

        return false;
    }

    override bool doCatch(void* entry, _Unwind_Exception* exceptionObject)
    {
        auto exception_class = exceptionObject.exception_class;
        if (ABI_libcxx.is_exception_class(exception_class))
            return doCatchImpl!ABI_libcxx(entry, exceptionObject);
        else if (ABI_libstdcxx.is_exception_class(exception_class))
            return doCatchImpl!ABI_libstdcxx(entry, exceptionObject);
        return false;
    }
}

// shared static this()
extern(C) void _d_cpp_init_eh()
{
    foreignHandlers ~=  new CppHandler;
}

} // !CRuntime_Microsoft
