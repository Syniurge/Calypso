// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/calypso.h"
#include "cpp/ddmdstructor.h"
#include "expression.h"
#include "identifier.h"
#include "scope.h"

#include "llvm/Support/FileSystem.h" // shall be replaced by std.file
#include "clang/Basic/Diagnostic.h"

namespace cpp
{

Import::Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId, int isstatic)
{
    construct_Import(this, loc, packages, id, aliasId, isstatic);
    // add "§cpp" as leftmost package to avoid name clashes
    if (!this->packages)
        this->packages = new Identifiers;
    this->packages->shift(calypso.id_Scpp);

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
        Scope* scx = Scope::createGlobal(::Module::rootModule);
        auto packages = new Identifiers;
        packages->push(idPool("cpp"));
        im_cpp_core = new_Import(Loc(), packages, idPool("core"), nullptr, true);
        im_cpp_core->importAll(scx);
        scx->pop();
        return;
    }

    ::Import::load(sc);
}

Modmap::Modmap(Loc loc, StringExp *arg)
{
    construct_Modmap(this, loc, arg);
}

void Modmap::importAll(Scope *sc)
{
    assert(arg->sz == 1); // FIXME
    calypso.pch.add(arg->toPtr(), sc->module);
}

void Modmap::semantic(Scope* sc)
{
}

GlobalImport::GlobalImport(Loc loc)
{
    construct_Import(this, loc, nullptr, calypso.id_Scpp, nullptr, 0);
    this->pkg = cpp::Module::rootPackage;
}

void GlobalImport::load(Scope *sc)
{
    if (loaded)
        return;

    sc->scopesym->importScope(pkg, Prot(PROTprivate));
    loaded = true;
}

}
