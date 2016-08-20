// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPMODULE_H
#define DMD_CPP_CPPMODULE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "dsymbol.h"
#include "module.h"
#include "cpp/calypso.h"

namespace clang
{
class Decl;
}

namespace cpp {

class Module : public ::Module
{
public:
    CALYPSO_LANGPLUGIN

    typedef std::pair<const clang::Decl *, const clang::Module *> RootKey;
    RootKey rootKey;

    static Package *rootPackage;    // package to store all C++ packages/modules, avoids name clashes (e.g std)
    static Modules amodules;            // array of all modules
    static void init();

    Module(const char *filename, Identifier *ident, Identifiers *packages);

    static Module *load(Loc loc, Identifiers *packages, Identifier *ident);
    Dsymbol *search(Loc loc, Identifier *ident, int flags = IgnoreNone) override;
    void addPreambule() override;
    const char *manglePrefix() override { return "_Cpp"; }
    bool isCodegen() override { return true; }

    File* buildFilePath(const char* forcename, const char* path, const char* ext) override;
};

bool isTemplateParameterPack(const clang::NamedDecl *Param);

}

#endif /* DMD_CPP_CPPMODULE_H */
