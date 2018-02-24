// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out

/**
 * libc stress import test
 */

modmap (C++) "<assert.h>";
modmap (C++) "<complex.h>";
modmap (C++) "<ctype.h>";
modmap (C++) "<errno.h>";
modmap (C++) "<fenv.h>";
modmap (C++) "<inttypes.h>";
modmap (C++) "<limits.h>";
modmap (C++) "<locale.h>";
modmap (C++) "<math.h>";
modmap (C++) "<setjmp.h>";
modmap (C++) "<signal.h>";
modmap (C++) "<stdint.h>";
modmap (C++) "<stdio.h>";
modmap (C++) "<stdlib.h>";
modmap (C++) "<string.h>";
modmap (C++) "<tgmath.h>";
modmap (C++) "<time.h>";
modmap (C++) "<uchar.h>";
modmap (C++) "<wchar.h>";
modmap (C++) "<wctype.h>";

import (C++) _;

void main()
{
    assert(FP_INFINITE == ℂFP_INFINITE);
    assert(FP_NORMAL == ℂFP_NORMAL);
}
