// CALYPSO: To avoid depending on libc++ and/or libstdc++, the parts used by Calypso were copied here
// and are now self-contained. Benefits outweigh the cons (future ABI changes).

#pragma once

#include <stdint.h>

typedef __SIZE_TYPE__ size_t;
typedef int _Atomic_word;

typedef uintptr_t _Unwind_Word;
typedef uintptr_t _Unwind_Ptr;
typedef uint64_t _Unwind_Exception_Class;

struct _Unwind_Context;
struct _Unwind_Exception;
typedef enum {
  _URC_NO_REASON = 0,
#if defined(__arm__) && !defined(__USING_SJLJ_EXCEPTIONS__) && \
    !defined(__ARM_DWARF_EH__)
  _URC_OK = 0, /* used by ARM EHABI */
#endif
  _URC_FOREIGN_EXCEPTION_CAUGHT = 1,

  _URC_FATAL_PHASE2_ERROR = 2,
  _URC_FATAL_PHASE1_ERROR = 3,
  _URC_NORMAL_STOP = 4,

  _URC_END_OF_STACK = 5,
  _URC_HANDLER_FOUND = 6,
  _URC_INSTALL_CONTEXT = 7,
  _URC_CONTINUE_UNWIND = 8,
#if defined(__arm__) && !defined(__USING_SJLJ_EXCEPTIONS__) && \
    !defined(__ARM_DWARF_EH__)
  _URC_FAILURE = 9 /* used by ARM EHABI */
#endif
} _Unwind_Reason_Code;

typedef enum {
  _UA_SEARCH_PHASE = 1,
  _UA_CLEANUP_PHASE = 2,

  _UA_HANDLER_FRAME = 4,
  _UA_FORCE_UNWIND = 8,
  _UA_END_OF_STACK = 16 /* gcc extension to C++ ABI */
} _Unwind_Action;

typedef void (*_Unwind_Exception_Cleanup_Fn)(_Unwind_Reason_Code,
                                             struct _Unwind_Exception *);

struct _Unwind_Exception {
  _Unwind_Exception_Class exception_class;
  _Unwind_Exception_Cleanup_Fn exception_cleanup;
  _Unwind_Word private_1;
  _Unwind_Word private_2;
  /* The Itanium ABI requires that _Unwind_Exception objects are "double-word
   * aligned".  GCC has interpreted this to mean "use the maximum useful
   * alignment for the target"; so do we. */
#if !defined(__APPLE__)
} __attribute__((__aligned__));
#else // defined(APPLE)
}; // CALYPSO NOTE: on Apple OSes, libc++abi is built against an unwind.h header that preserves default alignment for _Unwind_Exception.
   //  This results in a curious mismatch with the unwind.h header that comes with Clang, and a different layout for __cxa_exception.
#endif

namespace __cxxabiv1 {

// CALYPSO NOTE: The interesting part of the vtable of std::type_info differs only slightly.

class _type_info_common {
    virtual ~_type_info_common();

    virtual bool __is_pointer_p() const = 0;
    virtual void noop2() const = 0;
};

namespace libcxx {

class type_info : public _type_info_common { // libc++ equivalent: __shim_type_info
public:
    virtual ~type_info();
    virtual bool can_catch(const type_info *thrown_type,
                                    void **adjustedPtr) const = 0;
};

static const uint64_t kOurExceptionClass          = 0x434C4E47432B2B00; // CLNGC++\0
static const uint64_t kOurDependentExceptionClass = 0x434C4E47432B2B01; // CLNGC++\1
static const uint64_t get_vendor_and_language     = 0xFFFFFFFFFFFFFF00; // mask for CLNGC++

struct __cxa_exception {
#if defined(__LP64__) || defined(_LIBCXXABI_ARM_EHABI)
    // This is a new field to support C++ 0x exception_ptr.
    // For binary compatibility it is at the start of this
    // struct which is prepended to the object thrown in
    // __cxa_allocate_exception.
    size_t referenceCount;
#endif

    //  Manage the exception object itself.
    type_info *exceptionType;
    void (*exceptionDestructor)(void *);
    /*std::unexpected_handler*/ void* unexpectedHandler;
    /*std::terminate_handler*/  void* terminateHandler;

    __cxa_exception *nextException;

    int handlerCount;

#if defined(_LIBCXXABI_ARM_EHABI)
    __cxa_exception* nextPropagatingException;
    int propagationCount;
#else
    int handlerSwitchValue;
    const unsigned char *actionRecord;
    const unsigned char *languageSpecificData;
    void *catchTemp;
    void *adjustedPtr;
#endif

#if !defined(__LP64__) && !defined(_LIBCXXABI_ARM_EHABI)
    // This is a new field to support C++ 0x exception_ptr.
    // For binary compatibility it is placed where the compiler
    // previously adding padded to 64-bit align unwindHeader.
    size_t referenceCount;
#endif
    _Unwind_Exception unwindHeader;
};

// http://sourcery.mentor.com/archives/cxx-abi-dev/msg01924.html
// The layout of this structure MUST match the layout of __cxa_exception, with
// primaryException instead of referenceCount.
struct __cxa_dependent_exception {
#if defined(__LP64__) || defined(_LIBCXXABI_ARM_EHABI)
    void* primaryException;
#endif

    type_info *exceptionType;
    void (*exceptionDestructor)(void *);
    /*std::unexpected_handler*/ void* unexpectedHandler;
    /*std::terminate_handler*/  void* terminateHandler;

    __cxa_exception *nextException;

    int handlerCount;

#if defined(_LIBCXXABI_ARM_EHABI)
    __cxa_exception* nextPropagatingException;
    int propagationCount;
#else
    int handlerSwitchValue;
    const unsigned char *actionRecord;
    const unsigned char *languageSpecificData;
    void * catchTemp;
    void *adjustedPtr;
#endif

#if !defined(__LP64__) && !defined(_LIBCXXABI_ARM_EHABI)
    void* primaryException;
#endif
    _Unwind_Exception unwindHeader;
};

static
inline
__cxa_exception*
cxa_exception_from_thrown_object(void* thrown_object)
{
    return static_cast<__cxa_exception*>(thrown_object) - 1;
}

//  Get the exception object from the unwind pointer.
//  Relies on the structure layout, where the unwind pointer is right in
//  front of the user's exception object
static
inline
__cxa_exception*
cxa_exception_from_exception_unwind_exception(_Unwind_Exception* unwind_exception)
{
    return cxa_exception_from_thrown_object(unwind_exception + 1 );
}

static
void*
get_thrown_object_ptr(_Unwind_Exception* unwind_exception)
{
    // Even for foreign exceptions, the exception object is *probably* at unwind_exception + 1
    //    Regardless, this library is prohibited from touching a foreign exception
    void* adjustedPtr = unwind_exception + 1;
    if (unwind_exception->exception_class == kOurDependentExceptionClass)
        adjustedPtr = ((__cxa_dependent_exception*)adjustedPtr - 1)->primaryException;
    return adjustedPtr;
}

}  // libcxx

namespace libstdcxx {

class type_info : public _type_info_common { // libstdc++ equivalent: std::type_info
public:
    virtual ~type_info();
    virtual bool can_catch(const type_info *thrown_type,
                                    void **adjustedPtr,
                                    unsigned __outer = 1) const = 0;
};

struct __cxa_exception
{
  // Manage the exception object itself.
  type_info *exceptionType;
  void (/*_GLIBCXX_CDTOR_CALLABI*/ *exceptionDestructor)(void *);

  // The C++ standard has entertaining rules wrt calling set_terminate
  // and set_unexpected in the middle of the exception cleanup process.
    /*std::unexpected_handler*/ void* unexpectedHandler;
    /*std::terminate_handler*/  void* terminateHandler;

  // The caught exception stack threads through here.
  __cxa_exception *nextException;

  // How many nested handlers have caught this exception.  A negated
  // value is a signal that this object has been rethrown.
  int handlerCount;

#ifdef __ARM_EABI_UNWINDER__
  // Stack of exceptions in cleanups.
  __cxa_exception* nextPropagatingException;

  // The number of active cleanup handlers for this exception.
  int propagationCount;
#else
  // Cache parsed handler data from the personality routine Phase 1
  // for Phase 2 and __cxa_call_unexpected.
  int handlerSwitchValue;
  const unsigned char *actionRecord;
  const unsigned char *languageSpecificData;
  _Unwind_Ptr catchTemp;
  void *adjustedPtr;
#endif

  // The generic exception header.  Must be last.
  _Unwind_Exception unwindHeader;
};

struct __cxa_refcounted_exception
{
  // Manage this header.
  _Atomic_word referenceCount;
  // __cxa_exception must be last, and no padding can be after it.
  __cxa_exception exc;
};

// A dependent C++ exception object consists of a wrapper around an unwind
// object header with additional C++ specific information, containing a pointer
// to a primary exception object.

struct __cxa_dependent_exception
{
  // The primary exception this thing depends on.
  void *primaryException;

  // Unused member to get similar layout to __cxa_exception, otherwise the
  // alignment requirements of _Unwind_Exception would require padding bytes
  // before the unwindHeader member.
  void (/*_GLIBCXX_CDTOR_CALLABI*/ *__padding)(void *);

  // The C++ standard has entertaining rules wrt calling set_terminate
  // and set_unexpected in the middle of the exception cleanup process.
    /*std::unexpected_handler*/ void* unexpectedHandler;
    /*std::terminate_handler*/  void* terminateHandler;

  // The caught exception stack threads through here.
  __cxa_exception *nextException;

  // How many nested handlers have caught this exception.  A negated
  // value is a signal that this object has been rethrown.
  int handlerCount;

#ifdef __ARM_EABI_UNWINDER__
  // Stack of exceptions in cleanups.
  __cxa_exception* nextPropagatingException;

  // The number of active cleanup handlers for this exception.
  int propagationCount;
#else
  // Cache parsed handler data from the personality routine Phase 1
  // for Phase 2 and __cxa_call_unexpected.
  int handlerSwitchValue;
  const unsigned char *actionRecord;
  const unsigned char *languageSpecificData;
  _Unwind_Ptr catchTemp;
  void *adjustedPtr;
#endif

  // The generic exception header.  Must be last.
  _Unwind_Exception unwindHeader;
};

// Acquire the C++ exception header from the generic exception header.
static inline __cxa_exception *
__get_exception_header_from_ue (_Unwind_Exception *exc)
{
  return reinterpret_cast<__cxa_exception *>(exc + 1) - 1;
}

// Only checks for primary or dependent, but not that it is a C++ exception at
// all.
static inline bool
__is_dependent_exception(_Unwind_Exception_Class c)
{
  return (c & 1);
}

static inline __cxa_dependent_exception *
__get_dependent_exception_from_ue (_Unwind_Exception *exc)
{
  return reinterpret_cast<__cxa_dependent_exception *>(exc + 1) - 1;
}

static inline void*
__get_object_from_ue(_Unwind_Exception* eo)/* throw()*/
{
  return __is_dependent_exception (eo->exception_class) ?
    __get_dependent_exception_from_ue (eo)->primaryException :
    eo + 1;
}

#ifdef __ARM_EABI_UNWINDER__
static inline bool
__is_gxx_exception_class(_Unwind_Exception_Class c)
{
  // TODO: Take advantage of the fact that c will always be word aligned.
  return c[0] == 'G'
	 && c[1] == 'N'
	 && c[2] == 'U'
	 && c[3] == 'C'
	 && c[4] == 'C'
	 && c[5] == '+'
	 && c[6] == '+'
	 && (c[7] == '\0' || c[7] == '\x01');
}
#else // !__ARM_EABI_UNWINDER__
// This is the primary exception class we report -- "GNUCC++\0".
const _Unwind_Exception_Class __gxx_primary_exception_class
= ((((((((_Unwind_Exception_Class) 'G'
	 << 8 | (_Unwind_Exception_Class) 'N')
	<< 8 | (_Unwind_Exception_Class) 'U')
       << 8 | (_Unwind_Exception_Class) 'C')
      << 8 | (_Unwind_Exception_Class) 'C')
     << 8 | (_Unwind_Exception_Class) '+')
    << 8 | (_Unwind_Exception_Class) '+')
   << 8 | (_Unwind_Exception_Class) '\0');

// This is the dependent (from std::rethrow_exception) exception class we report
// "GNUCC++\x01"
const _Unwind_Exception_Class __gxx_dependent_exception_class
= ((((((((_Unwind_Exception_Class) 'G'
	 << 8 | (_Unwind_Exception_Class) 'N')
	<< 8 | (_Unwind_Exception_Class) 'U')
       << 8 | (_Unwind_Exception_Class) 'C')
      << 8 | (_Unwind_Exception_Class) 'C')
     << 8 | (_Unwind_Exception_Class) '+')
    << 8 | (_Unwind_Exception_Class) '+')
   << 8 | (_Unwind_Exception_Class) '\x01');

static inline bool
__is_gxx_exception_class(_Unwind_Exception_Class c)
{
  return c == __gxx_primary_exception_class
      || c == __gxx_dependent_exception_class;
}
#endif // !__ARM_EABI_UNWINDER__

}  // libstdcxx

}  // __cxxabiv1
