// RUN: %ldc -cpp-cachedir=%t.cache -of %t %s
// RUN: %t

modmap (C++) "struct_ctor.h";

import (C++) S258, S510, S772, S1020;

void main()
{
    {
        S258 a;
        assert(a.n123 == 0);
        assert(a.n321 == 555);

        S510 b;
        assert(b.n753 == 0);

        S772 c;
        assert(c.n951 == 998877);

        S1020 d;
        assert(d.n8079 == 111101);
    }
    {
        auto a = new S258;
        assert(a.n123 == 0);
        assert(a.n321 == 555);

        auto b = new S510;
        assert(b.n753 == 0);

        auto c = new S772;
        assert(c.n951 == 998877);

        auto d = new S1020;
        assert(d.n8079 == 111101);
    }
}
