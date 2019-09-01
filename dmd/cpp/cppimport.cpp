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
    // add "ℂcpp" as leftmost package to avoid name clashes
    if (!this->packages)
        this->packages = new Identifiers;
    this->packages->shift(calypso.id_Scpp);

    if (!aliasId)
        setSymIdent();
}

::Module* Import::loadModule(Scope* sc)
{
    calypso.pch.update();
    
    bool isTypedef;
    ::Module* m = Module::load(loc, packages, id, isTypedef);

    if (isTypedef) {
        if (!aliasId)
            aliasId = id;
        if (!isstatic && !names.dim) {
            addAlias(id, nullptr);

            // re-do either Import.addMember or visit(ImportStatement imps) (ugly code duplication, but minimally intrusive..)
            auto tname = new_TypeIdentifier(loc, id);
            auto ad = new_AliasDeclaration(loc, id, tname);
            ad->_import = this;
            aliasdecls.push(ad);
            if (parent) {
                ad->addMember(sc, static_cast<ScopeDsymbol*>(parent));
                ad->setScope(sc);
            } else {
                sc->insert(ad);
            }
        }
        id = calypso.id__;
        load(sc);
        m = mod;
    }

    if (!m->importedFrom)
        m->importedFrom = getInstantiatingModule();

    return m;
}

}
