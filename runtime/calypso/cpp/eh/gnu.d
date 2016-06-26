/**
 * This module implements ...
 */
module cpp.eh.gnu;

modmap (C++) "unwind-cxx.h";

import ldc.eh.common;
import ldc.eh.libunwind;

import (C++) __cxxabiv1._;
import (C++) __cxxabiv1.__cxa_exception;
import (C++) _Unwind_Exception : _cpp__Unwind_Exception = _Unwind_Exception;

import (C++) std.type_info;

class CppHandler : ForeignHandler
{
    __cxa_exception *_cpp_exception;
    _Unwind_Context_Ptr context;

    this(_cpp__Unwind_Exception *e, _Unwind_Context_Ptr context)
    {
        this._cpp_exception = __get_exception_header_from_ue(e);
        this.context = context;
    }

    void *getException()
    {
        return &_cpp_exception.unwindHeader;
    }

    type_info *getCatchTypeInfo(void* address, ubyte encoding)
    {
        size_t catchTypeInfoWrapAddr;
        get_encoded_value(cast(ubyte*) address, catchTypeInfoWrapAddr, encoding, context);

        auto a = cast(__cpp_type_info_ptr)cast(Object)cast(void*)catchTypeInfoWrapAddr;
        return a ? cast(type_info*)a.p : null;
    }

    bool doCatch(void* address, ubyte encoding)
    {
        void *__thr_obj = __get_object_from_ue(&_cpp_exception.unwindHeader);

        // Pointer types need to adjust the actual pointer, not the pointer to pointer that is the exception object.
        // This also has the effect of passing pointer types "by value" through the __cxa_begin_catch return value.
        if (_cpp_exception.exceptionType.__is_pointer_p())
            __thr_obj = *cast(void **) __thr_obj;

        auto catchTypeInfo = getCatchTypeInfo(address, encoding);
        if (catchTypeInfo && catchTypeInfo.__do_catch(_cpp_exception.exceptionType, & __thr_obj, 1))
        {
            _cpp_exception.adjustedPtr = __thr_obj; // NOTE: __cxa_begin_catch returns adjustedPtr, which in C++ EH is set by __gxx_personality_v0 if the search phase is successful
            return true;
        }

        return false;
    }
}

class CppHandlerFactory : ForeignHandlerFactory
{
    bool doHandleExceptionClass(ulong exception_class) shared
    {
        return __is_gxx_exception_class(exception_class);
    }

    ForeignHandler create(_Unwind_Context_Ptr context, _Unwind_Exception* exception_info) shared
    {
        return new CppHandler(cast(_cpp__Unwind_Exception*) exception_info, context);
    }
}

shared static this()
{
    foreignHandlerFactories ~=  new CppHandlerFactory;
}
