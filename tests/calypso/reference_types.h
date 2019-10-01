#pragma once

class ZString
{
public:
    const char* data;
    unsigned len;

    ZString(const char* str) : data(str), len(0) {}
};

void Foo6520(const ZString& s) { ZString o(s.data); }
