// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
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
    // add "cpp" as leftmost package to avoid name clashes
    if (!this->packages)
        this->packages = new Identifiers;
    this->packages->shift(Identifier::idPool("cpp"));  // any better idea ?

    if (!aliasId)
        setSymIdent();
}

::Module* Import::loadModule(Loc loc, Identifiers* packages, Identifier* id)
{
    calypso.pch.update();
    
    return Module::load(loc, packages, id);
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
