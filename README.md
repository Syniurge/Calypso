Calypso notes
------------

Calypso creates a bridge between DMD/LDC and Clang, both at the AST level (DMD <=> Clang's AST, Sema, ...) and at the code generation level (LDC <=> Clang's Codegen) to make D interface directly with the almost full set of C++ features, and no binding is needed.

It's not a separate tool, but a fork of LDC which enables you to directly import/include a C/C++ header and use the declarations from within D. No intermediate file is necessary.

Calypso introduces a new keyword, **modmap**, along with the concept of language plugins which are queried by DMD's parser when it encounters special « **import** *(ABC)* xxx.yyy; » symbols. Interfacing with C++ declarations comes down to:

```D
modmap (C++) "cppheader.h";      // tells Clang to load cppheader.h but do not import anything

import (C++) NamespaceA.Class1;  // imports NamespaceA::Class1
import (C++) NamespaceA._;       // special module per namespace, imports every global variables,
                                 // global functions and typedefs whose direct parent is NamespaceA::
```

The resulting imported symbols are usable like their D counterparts. For more detailed examples and explanations on Calypso's features see [tests/calypso](tests/calypso).

Although Calypso is currently soldered to LDC, separating the two and placing Calypso and its bulky Clang dependency in an optional shared library should be easy. In this way, D compilers won't have to depend on a C/C++ compiler, and wider C++ support than what D currently has won't result in too cumbersome intrusions in core DMD/LDC.

Installation notes
-------

A Clang 3.9 fork makes its appearance as a submodule, it's therefore strongly recommended to build Calypso against LLVM 3.9 (also, the nightly 3.9 build, not the 3.9.0 release which contains a breaking bug).

Please note that to build Calypso in ```Debug``` mode LLVM needs to be built in ```Debug``` mode as well.

Specific flags and building the basic example
-------

Calypso adds the -cpp-flags option to LDC to pass arguments to Clang during header parsing, e.g to enable C++11 while building [tests/calypso/basic/basics.d](tests/calypso/basic/basics.d):

    $ clang++ -std=c++11 -c basics.cpp -o basics.cpp.o
    $ ldc2 -cpp-args -std=c++11 basics.cpp.o -L-lstdc++ basics.d

LDC – the LLVM-based D Compiler
===============================

[![Build Status](https://travis-ci.org/ldc-developers/ldc.png?branch=master)][1]
[![Test Coverage](https://coveralls.io/repos/ldc-developers/ldc/badge.svg)][2]
[![Bountysource](https://www.bountysource.com/badge/tracker?tracker_id=283332)][3]

The LDC project aims to provide a portable D programming language
compiler with modern optimization and code generation capabilities.

The compiler uses the official DMD frontends to support the latest
version of D2, and relies on the LLVM Core libraries for code
generation.

LDC is fully Open Source; the parts of the code not taken/adapted from
other projects are BSD-licensed (see the LICENSE file for details).

Please consult the D wiki for further information:
http://wiki.dlang.org/LDC

D1 is no longer available; see the 'd1' Git branch for the last
version supporting it.


Installation
------------

In-depth material on building and installing LDC and the standard
libraries, including experimental instructions for running LDC on
Windows, is available on the project wiki at
http://wiki.dlang.org/Building_LDC_from_source.

If you have a working C++ build environment, CMake, a current LLVM, and
[libconfig](http://hyperrealm.com/libconfig/libconfig.html) available
there should be no big surprises, though.

Do not forget to make sure all the submodules are up to date:

    $ cd ldc
    $ git submodule update --init

Some Linux and OS X package managers include recent versions of LDC, so
manually installing it might not be necessary. For several platforms,
there are also stand-alone binary builds available at the
[GitHub release page](https://github.com/ldc-developers/ldc/releases).


Contact
-------

The best way to get in touch with the developers is either via the
digitalmars.D.ldc forum/newsgroup/mailing list
(http://forum.dlang.org) or the #ldc IRC channel on FreeNode.

For further documentation, contributor information, etc. please see
the D wiki: http://wiki.dlang.org/LDC

Feedback of any kind is very much appreciated!


[1]: https://travis-ci.org/ldc-developers/ldc "Build Status"
[2]: https://coveralls.io/r/ldc-developers/ldc "Test Coverage"
[3]: https://www.bountysource.com/trackers/283332-ldc?utm_source=283332&utm_medium=shield&utm_campaign=TRACKER_BADGE "Bountysource"
