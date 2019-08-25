// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPMODULE_H
#define DMD_CPP_CPPMODULE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root/root.h"
#include "arraytypes.h"
#include "dsymbol.h"
#include "module.h"
#include "cpp/calypso.h"

#include <map>
#include <unordered_set>

namespace clang
{
class Decl;
}

namespace cpp {

class DeclMapper;

class Module : public ::Module
{
public:
    CALYPSO_LANGPLUGIN

    const clang::Decl *rootDecl;

    TemplateInstances tempinsts; // not added to members, but listed here to get emitted

    static std::map<const clang::Decl *, cpp::Module*> allCppModules;

    // List of C++ symbols emitted in the existing object file
    // If a C++ symbol not in this list was referenced, the module needs to be re-gen'd
    std::unordered_set<std::string> emittedSymbols;
    bool needGen = false;

    static Package *rootPackage;    // package to store all C++ packages/modules, avoids name clashes (e.g std)
    static Modules amodules;            // array of all modules
    static void init();

    Module(const char* filename, Identifier* ident);

    static Module *get(const clang::Decl* rootDecl);
    static Package *getPackage(const clang::Decl* NSorTU);

    static Module *load(Loc loc, Identifiers *packages, Identifier *id, bool& isTypedef);
    Dsymbol *search(const Loc& loc, Identifier *ident, int flags = IgnoreNone) override;
    void complete() override;

    void addPreambule() override;
    const char *manglePrefix() override { return "_Cpp"; }
    bool isCodegen() override { return true; } // §cpp modules aren't root, but they are codegen'd so non-instantiated functions need to be emitted

    void loadEmittedSymbolList();
    void saveEmittedSymbolList();

protected:
    bool searchedInlineNamespaces = false; // NOTE: they may need to get refreshed after a new cppmap
    std::vector<const clang::NamespaceDecl*> inlineNamespaces;

    struct NonMemberOverloadedOperators
    {
        std::vector<const clang::Decl*> OOs;
        bool searched = false;
    };
    std::map<clang::OverloadedOperatorKind, NonMemberOverloadedOperators> nonMemberOverloadedOperators;

    void searchInlineNamespaces();
    void searchNonMemberOverloadedOperators(clang::OverloadedOperatorKind Op); // map non-member overloaded operators and out-of-line friend decls
};

bool isTemplateParameterPack(const clang::NamedDecl *Param);

}

#endif /* DMD_CPP_CPPMODULE_H */
