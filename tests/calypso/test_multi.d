// RUN: %ldc -cpp-args -std=c++11 -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

/+
D20180122T194048:here This file is intended to be used for multiple tests when feasible (as opposed to 1 file per test), to lower coding cost of adding a test and encouraging higher test coverage.
+/

modmap (C++) "test_multi.h";

void main() {
	{
		/+
		https://github.com/Syniurge/Calypso/issues/65
		fixes:
		Error: template instance opCast!(A) does not match template declaration opCast(type : float)
		+/
		import (C++) issue_65.A, issue_65.B;
		int a=B.init.a;
	}
}

