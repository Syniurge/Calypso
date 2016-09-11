module cpp.std.range;

struct STLInputRange(T)
{
    T.iterator it, iend;

    this(ref T container) {
        this.it = container.begin();
        this.iend = container.end();
    }

    @property bool empty() const {
        return it == iend;
    }

    @property ref T.value_type front() {
        return *it;
    }

    void popFront() {
        it++;
    }
}

STLInputRange!T irange(T)(ref T container) {
    return STLInputRange!T(container);
}
