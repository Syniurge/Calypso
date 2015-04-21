rm -f calypso_cache* *.o
clang++ -std=c++11 -c showcase.cpp -o showcase.cpp.o
ar rcs libshowcase.a showcase.cpp.o
ldc2 -L-lstdc++ showcase.d -Llibshowcase.a -L-lsupc++
