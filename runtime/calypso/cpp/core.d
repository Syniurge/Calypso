module cpp.core;

version (CRuntime_Microsoft) {} else
{

extern(C) void _d_cpp_init_eh(); // HACK to avoid importing cpp.eh.gnu
shared static this()
{
    _d_cpp_init_eh();
}

} // !CRuntime_Microsoft
