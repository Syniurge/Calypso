While loading a precompiled header Calypso looks for *.modulemap_d files in the header folders.
They share the same format as Clang module map files (http://clang.llvm.org/docs/Modules.html) although they are used differently, hence the different extension. They can be generated with the clang-modularize tool (in clang-tools-extra).

Without those module maps Calypso puts every global function, variable and typedefs into the "_" module, which lead to name clashes in the libc because C/C++ allows global variables and functions to share the same name whereas D doesn't. So module maps are essential to split densely populated namespaces and avoid name conflicts.

Currently provided are module maps for GNU's libc headers, which on Linux or MingW should be copied into /usr/include:

  $ cp -R libstdc-gnu.modulemap_d posix.modulemap_d sys /usr/include/
  
Later once more module maps are made for more environments libc module maps will be installed alongside Calypso.
