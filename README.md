Calypso notes
------------

Calypso creates a bridge between DMD/LDC and Clang, both at the AST level (DMD <=> Clang's AST, Sema, ...) and at the code generation level (LDC <=> Clang's Codegen) to make D interface directly with the almost entire spectrum of C++ features.

It's not a separate tool, but a fork of LDC which enables you to directly import/include a C/C++ header and use the declarations from within D. No intermediate file is necessary, and no binding is involved.

Calypso introduces a new pragma, **cppmap**, along with the concept of language plugins which are queried by DMD's parser when it encounters special « **import** *(ABC)* xxx.yyy; » symbols. Interfacing with C++ declarations comes down to:

```D
pragma (cppmap, "cppheader.h");  // tells Clang to parse cppheader.h but do not import anything

import (C++) NamespaceA.Class1;  // imports NamespaceA::Class1
import (C++) NamespaceA._;       // special module per namespace, imports every global variables,
                                 // global functions and typedefs whose direct parent is NamespaceA::
```

The resulting imported symbols are usable like their D counterparts. For more detailed examples see [tests/calypso](tests/calypso) and [examples](examples).

Although Calypso is currently soldered to LDC, separating the two and placing Calypso and its bulky Clang dependency in an optional shared library should be easy. In this way, D compilers won't have to depend on a C/C++ compiler, and wider C++ support than what D currently has won't result in too cumbersome intrusions in core DMD/LDC.

Installation notes
-------

Refer to the LDC wiki ([Linux and OS X](http://wiki.dlang.org/Building_LDC_from_source) and
[Windows](http://wiki.dlang.org/Building_and_hacking_LDC_on_Windows_using_MSVC)).

There are only a few requirements specific to Calypso:
- CMake minimum version is 3.4+
- Clang is tied to its corresponding LLVM version, it's therefore strongly recommended to build Calypso which introduces a Clang 5.0 submodule against LLVM 5.0
- MSVC (Windows) builds require [Ninja](https://ninja-build.org/)

`cmake -G Ninja` is recommended instead of `make` to take advantage by default of all available CPU cores while building the Clang external module (it's also possible to set `cmake -DCLANG_BUILD_FLAGS="-jX"`).

Calypso may also have nightly builds in the future.

## Specific flags

The `-cpp-flags` option was added to LDC to pass arguments to Clang during header parsing, e.g to enable C++11:

    $ ldc2 -cpp-args -std=c++11 main.d

## Missing features

### Exception handling
* C++ exception rethrowing
* `catch (C++) {}` i.e catch any C++ exception instead of specifying a type with `catch (C++) (ref T e)` (https://github.com/Syniurge/Calypso/issues/74)
* MSVC exception handling

### Other
* Register the destructor of C++ classes and structs while allocating a C++ class through the GC (as is being done for D structs)
* Map macro functions as enums and templates (https://github.com/Syniurge/Calypso/issues/66). Currently only macro constants get mapped.

*****

LDC – the LLVM-based D Compiler
===============================

[![Build Status](https://circleci.com/gh/ldc-developers/ldc/tree/master.svg?style=svg)][1]
[![Build Status](https://semaphoreci.com/api/v1/ldc-developers/ldc/branches/master/shields_badge.svg)][2]
[![Build Status](https://travis-ci.org/ldc-developers/ldc.png?branch=master)][3]
[![Build Status](https://ci.appveyor.com/api/projects/status/2cfhvg79782n4nth/branch/master?svg=true)][4]
[![Bountysource](https://www.bountysource.com/badge/tracker?tracker_id=283332)][5]

The LDC project provides a portable D programming language compiler
with modern optimization and code generation capabilities.

The compiler uses the official DMD frontend to support the latest
version of D2, and relies on the LLVM Core libraries for code
generation.

LDC is fully Open Source; the parts of the source code not taken/adapted from
other projects are BSD-licensed (see the LICENSE file for details).

Please consult the D wiki for further information:
https://wiki.dlang.org/LDC

D1 is no longer available; see the `d1` Git branch for the last
version supporting it.


Installation
------------

### From a pre-built package

Portable stand-alone binary builds for common platforms (incl. Linux,
macOS and Windows) are available at the
[GitHub release page](https://github.com/ldc-developers/ldc/releases).

For bleeding-edge users, we also provide the [latest successful
Continuous Integration builds](https://github.com/ldc-developers/ldc/releases/tag/CI)
with enabled LLVM & LDC assertions (increasing compile times by roughly 50%).

The [dlang.org install script](https://dlang.org/install.html)
can also be used to install LDC:

    curl -fsS https://dlang.org/install.sh | bash -s ldc

In addition, LDC is available from various package managers
(but note that these packages **might be outdated** as they are
not currently integrated into the project release process):

|              | Command                                      |
| ------------ | -------------------------------------------- |
| **Android**  | in [Termux app](https://play.google.com/store/apps/details?id=com.termux&hl=en): `pkg install ldc` |
| Arch Linux   | `pacman -S ldc`                              |
| Debian       | `apt install ldc`                            |
| Fedora       | `dnf install ldc`                            |
| Gentoo       | `layman -a ldc`                              |
| Homebrew     | `brew install ldc`                           |
| Ubuntu       | `apt install ldc`                            |
| Snap         | `snap install --classic --channel=edge ldc2` |
| Nix/NixOS    | `nix-env -i ldc`                             |
| Chocolatey   | `choco ldc`                                  |
| Docker       | `docker pull dlanguage/ldc`                  |

#### Targeting Android

You can find full instructions on cross-compiling or natively compiling
for Android [on the wiki](https://wiki.dlang.org/Build_D_for_Android).

### Building from source

In-depth material on building and installing LDC and the standard
libraries is available on the project wiki for
[Linux, macOS, BSD, and Android](http://wiki.dlang.org/Building_LDC_from_source) and
[Windows](http://wiki.dlang.org/Building_and_hacking_LDC_on_Windows_using_MSVC).

If you have a working C++/D build environment, CMake, and a current LLVM
version (≥ 3.9) available, there should be no big surprises. Do not
forget to make sure all the submodules (druntime, phobos, dmd-testsuite)
are up to date:

    $ cd ldc
    $ git submodule update --init

(DMD and LDC are supported as host compilers. For bootstrapping
purposes, LDC 0.17, the last version not to require a D compiler, is
maintained in the `ltsmaster` branch).

### Cross-compiling

We've recently added a cross-compilation tool to make it easier to build the D
runtime and standard library for other platforms, `ldc-build-runtime`. Full
instructions and example invocations are provided
[on its wiki page](https://wiki.dlang.org/Building_LDC_runtime_libraries).

Contact
-------

The best way to get in touch with the developers is either via the
[digitalmars.D.ldc forum/newsgroup/mailing list](https://forum.dlang.org)
or our [Gitter chat](http://gitter.im/ldc-developers/main).
There is also the #ldc IRC channel on FreeNode.

For further documentation, contributor information, etc. please see
[the D wiki](https://wiki.dlang.org/LDC).

Feedback of any kind is very much appreciated!


[1]: https://circleci.com/gh/ldc-developers/ldc/tree/master "Circle CI Build Status"
[2]: https://semaphoreci.com/ldc-developers/ldc "Semaphore CI Build Status"
[3]: https://travis-ci.org/ldc-developers/ldc "Travis CI Build Status"
[4]: https://ci.appveyor.com/project/kinke/ldc/history "AppVeyor CI Build Status"
[5]: https://www.bountysource.com/teams/ldc-developers/issues "Bountysource"
