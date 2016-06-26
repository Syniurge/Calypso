// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/calypso.h"
#include "expression.h"
#include "identifier.h"
#include "scope.h"

#include "llvm/Support/FileSystem.h" // shall be replaced by std.file
#include "clang/Basic/Diagnostic.h"

namespace cpp
{

Import::Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId, int isstatic)
    : ::Import(loc, packages, id, aliasId, isstatic)
{
    // add "_cpp" as leftmost package to avoid name clashes
    if (!this->packages)
        this->packages = new Identifiers;
    this->packages->shift(Identifier::idPool("_cpp"));  // any better idea ?

    if (!aliasId)
        setSymIdent();
}

::Module* Import::loadModule(Loc loc, Identifiers* packages, Identifier* id)
{
    calypso.pch.update();
    
    return Module::load(loc, packages, id);
}

void Import::load(Scope* sc)
{
    // Ugly HACK to anticipate the modmap from cpp.eh.gnu and prevent the AST merging assert
    static ::Import* im_cpp_core = nullptr;
    if (!im_cpp_core) {
        auto packages = new Identifiers;
        packages->push(Identifier::idPool("cpp"));
        im_cpp_core = new ::Import(Loc(), packages, Identifier::idPool("core"), nullptr, true);
        im_cpp_core->importAll(nullptr);
        return;
    }

    ::Import::load(sc);
}

Modmap::Modmap(Loc loc, StringExp *arg)
    : ::Modmap(loc, arg)
{
}

void Modmap::importAll(Scope *sc)
{
    assert(arg->sz == 1); // FIXME
    assert(strlen((const char *) arg->string) == arg->len);
    calypso.pch.add((const char *) arg->string, sc->module);
}

void Modmap::semantic(Scope* sc)
{
}

}
