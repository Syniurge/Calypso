#pragma once

struct S825
{
    int static_metacall(bool, int, void **) const;

    struct {
        int static_metacall;
    } d;
};
