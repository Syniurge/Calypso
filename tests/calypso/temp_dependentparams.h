#pragma once

struct Slong {
    typedef long int_type;
};

template<typename S,
        typename S::int_type IntTy>
class A {};
