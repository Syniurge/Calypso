// RUN: true

module cppmap2;

struct XYZ
{
    version (CPPMAP_C)
    {
        pragma (cppmap, "cppmap_c.h");
        import (C++) V4X.Z;
    }
}
