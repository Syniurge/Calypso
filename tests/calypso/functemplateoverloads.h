#pragma once

template<typename _Tp> _Tp foo(unsigned char v)    { return _Tp(v); }
template<typename _Tp> _Tp foo(signed char v)    { return _Tp(v); }
template<typename _Tp> _Tp foo(unsigned short v)   { return _Tp(v); }

template<> unsigned char foo<unsigned char>(signed char v)        { return (unsigned char) 0; }
template<> unsigned char foo<unsigned char>(unsigned short v)       { return (unsigned char) 1; }

template<> unsigned short foo<unsigned short>(unsigned char v)      { return (unsigned short) 2; }
