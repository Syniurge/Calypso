#pragma once

template <typename _Tp>
struct Point
{
    _Tp x;
    _Tp y;

    Point(_Tp x = 0, _Tp y = 0) : x(x), y(y) {}

    operator _Tp() const { return x*y; }
    template <typename _Tp2> operator Point<_Tp2>() const;
};

template<typename _Tp> template<typename _Tp2>
Point<_Tp>::operator Point<_Tp2>() const
{
    return Point<_Tp2>(_Tp2(x), _Tp2(y));
}
