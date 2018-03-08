// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t > %t.out

/**
 * libc stress import test
 */

pragma (cppmap, "<assert.h>");
pragma (cppmap, "<complex.h>");
pragma (cppmap, "<ctype.h>");
pragma (cppmap, "<errno.h>");
pragma (cppmap, "<fenv.h>");
pragma (cppmap, "<inttypes.h>");
pragma (cppmap, "<limits.h>");
pragma (cppmap, "<locale.h>");
pragma (cppmap, "<math.h>");
pragma (cppmap, "<setjmp.h>");
pragma (cppmap, "<signal.h>");
pragma (cppmap, "<stdint.h>");
pragma (cppmap, "<stdio.h>");
pragma (cppmap, "<stdlib.h>");
pragma (cppmap, "<string.h>");
pragma (cppmap, "<tgmath.h>");
pragma (cppmap, "<time.h>");
pragma (cppmap, "<uchar.h>");
pragma (cppmap, "<wchar.h>");
pragma (cppmap, "<wctype.h>");

import (C++) _;

void main()
{
    assert(FP_INFINITE == ℂFP_INFINITE);
    assert(FP_NORMAL == ℂFP_NORMAL);
}
