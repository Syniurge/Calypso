/*
see D20180122T194048
*/

#pragma once

#include "assert.h"

namespace issue_65{
	struct A {
		int a;
	  operator float() const;
	};

	struct B : public A{
	};
}
