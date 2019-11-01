#pragma once

enum EnumAAA
{
    AAA_1,
    AAA_2 = 512,
    AAA_3 = 8192,
};

enum EnumBBB : char
{
    BBB_1 = 'a',
    BBB_2,
    BBB_3,
    BBB_4,
    BBB_5,
};

enum
{
    Anon_1 = 2222,
    Anon_2 = 4444
};

enum class EnumClass
{
    EC_1,
    EC_2,
    EC_3
};

typedef enum
{
    TDEF_1,
    TDEF_2
} EnumTypedef;

namespace test
{
    enum EnumInner
    {
        EInn_1,
        EInn_2,
        EInn_3
    };

    enum class EnumClassInner
    {
        CI_1,
        CI_2,
        CI_3
    };
}

struct Struct
{
    enum
    {
        AnonInStruct_1,
        AnonInStruct_2
    };
};

struct Struct2
{
    enum
    {
        AIS_1,
        AIS_2
    };
};
