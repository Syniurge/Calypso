#pragma once

#include <utility>
#include "assert.h"

struct A {
  static bool assertCopyCtor;
  static bool assertMoveCtor;
  static bool assertMainCtor;
  static bool assertDtor;

  int x;

  A(const A &a) : A(a.x) {
    if (assertCopyCtor)
        assert(false);
  }

  A(A &&a) : A(a.x) {
    if (assertMoveCtor)
        assert(false);
  }

  A(int x) : x(x) {
    if (assertMainCtor)
        assert(false);
  }

  ~A() {
    if (assertDtor)
        assert(false);
  }

  static void bar(A&& a) noexcept {}
  static A&& ret(A&& a) noexcept { return std::move(a); }
};

bool A::assertCopyCtor = false;
bool A::assertMoveCtor = false;
bool A::assertMainCtor = false;
bool A::assertDtor = false;
