#pragma once

// #include <iostream>
#include "assert.h"

struct B {
  int x;
  int *y;

  // rule of 5:
  // https://en.wikipedia.org/wiki/Rule_of_three_(C%2B%2B_programming)
  B(const B &a) : B(a.x) {}

  B(B &&a) : B(a.x) {
    a.release();
    assert(false); // ensures that the test case isn't calling the move ctor
  }

  B &operator=(const B &a) {
    initialize(a.x);
    return *this;
  }

  B &operator=(B &&a) {
    if (this == &a)
      return *this;
    initialize(a.x);
    a.release();
    return *this;
  }

  B(int x) { initialize(x); }

  ~B() {}

  void release() {
    // could release owned resources here
  }

  void initialize(int x) {
    this->x = x;
    this->y = &this->x;
    test();
  }

  void test() {
//     std::cout << x << " " << y << " " << &x << " " << (&x == y) << std::endl;
    assert(&x == y);
  }
};
