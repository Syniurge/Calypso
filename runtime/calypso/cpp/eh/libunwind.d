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

import ldc.eh.common;
import ldc.eh.libunwind;

import cpp.core;

modmap (C++) "unwind-cxx-d.h";

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

    import (C++) __cxxabiv1.libcxx.type_info,
                 __cxxabiv1.libcxx.__cxa_exception,
                 __cxxabiv1.libcxx.__cxa_dependent_exception;
    import (C++) __cxxabiv1.libcxx._ : cxa_exception_from_exception_unwind_exception,
                                       get_thrown_object_ptr;
}

struct ABI_libstdcxx
{
    alias is_exception_class = __is_gxx_exception_class;

    import (C++) __cxxabiv1.libstdcxx.type_info,
                 __cxxabiv1.libstdcxx.__cxa_exception,
                 __cxxabiv1.libstdcxx.__cxa_dependent_exception;
    import (C++) __cxxabiv1.libstdcxx._ :
                        __is_gxx_exception_class,
                        cxa_exception_from_exception_unwind_exception = __get_exception_header_from_ue,
                        get_thrown_object_ptr = __get_object_from_ue;
}

class CppHandler(ABI) : ForeignHandler
{
    ABI.__cxa_exception *_cpp_exception;
    _Unwind_Context_Ptr context;

    this(_cpp__Unwind_Exception *e, _Unwind_Context_Ptr context)
    {
        this._cpp_exception = ABI.cxa_exception_from_exception_unwind_exception(e);
        this.context = context;
    }

    void *getException()
    {
        return &_cpp_exception.unwindHeader;
    }

    ABI.type_info *getCatchTypeInfo(void* address, ubyte encoding)
    {
        size_t catchTypeInfoWrapAddr;
        get_encoded_value(cast(ubyte*) address, catchTypeInfoWrapAddr, encoding, context);

        auto a = cast(__calypso_type_info_ptr)cast(Object)cast(void*)catchTypeInfoWrapAddr;
        return a ? cast(ABI.type_info*)a.p : null;
    }

    bool doCatch(void* address, ubyte encoding)
    {
        void *__thr_obj = ABI.get_thrown_object_ptr(&_cpp_exception.unwindHeader);

        // libstdc++ only: for pointer types it's the actual pointer that needs to get adjusted, not the pointer to pointer that is the exception object.
        // This also has the effect of passing pointer types "by value" through the __cxa_begin_catch return value.
        static if (is(ABI == ABI_libstdcxx))
            if (_cpp_exception.exceptionType.__is_pointer_p())
                __thr_obj = *cast(void **) __thr_obj;

        auto catchTypeInfo = getCatchTypeInfo(address, encoding);
        if (catchTypeInfo && catchTypeInfo.can_catch(_cpp_exception.exceptionType, &__thr_obj))
        {
            _cpp_exception.adjustedPtr = __thr_obj; // NOTE: __cxa_begin_catch returns adjustedPtr, which in C++ EH is set by the personality routine if the search phase is successful
            return true;
        }

        return false;
    }
}

class CppHandlerFactory : ForeignHandlerFactory
{
    bool doHandleExceptionClass(ulong exception_class) shared
    {
        return ABI_libcxx.is_exception_class(exception_class)
            || ABI_libstdcxx.is_exception_class(exception_class);
    }

    ForeignHandler create(_Unwind_Context_Ptr context, _Unwind_Exception* exception_info) shared
    {
        if (ABI_libcxx.is_exception_class(exception_info.exception_class))
            return new CppHandler!ABI_libcxx(cast(_cpp__Unwind_Exception*) exception_info, context);
        else
            return new CppHandler!ABI_libstdcxx(cast(_cpp__Unwind_Exception*) exception_info, context);
    }
}

// shared static this()
extern(C) void _d_cpp_init_eh()
{
    foreignHandlerFactories ~=  new CppHandlerFactory;
}

} // !CRuntime_Microsoft
