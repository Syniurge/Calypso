// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/calypso.h"
#include "cpp/ddmdstructor.h"
#include "errors.h"
#include "expression.h"
#include "identifier.h"
#include "scope.h"

#include "llvm/Support/FileSystem.h" // shall be replaced by std.file
#include "clang/Basic/Diagnostic.h"

namespace cpp
{

Import::Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId, int isstatic)
{
    if (!id) // special import(C++) *; to import the entire translation unit
        id = calypso.id_Scpp; // rename ℂcpp.* to ℂcpp
    else
    {
        // add "ℂcpp" as leftmost package to avoid name clashes
        if (!packages)
            packages = new Identifiers;
        packages->shift(calypso.id_Scpp);
    }

    construct_Import(this, loc, packages, id, aliasId, isstatic);
}

::Module* Import::loadModule(Scope* sc, Package* pparent)
{
    bool isTypedef;
    ::Module* m = Module::load(loc, pparent, id, isTypedef);

    if (isTypedef) {
        if (!aliasId)
            aliasId = id;
        if (!isstatic && !names.dim) {
            addAlias(id, nullptr);

            // re-do either Import.addMember or visit(ImportStatement imps)
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

bool Import::load(Scope* sc)
{
    auto errors = global.errors;

    calypso.pch.update();
    if (!Module::rootPackage)
        Module::init();

    ::Package* pparent;
    DsymbolTable* dst = Package::tryResolve(loc, packages, &pparent, &pkg);
    if (!dst)
        return true;

    assert(!pparent || isCPP(pparent));
    auto c_pparent = pparent ? static_cast<cpp::Package*>(pparent) : nullptr;

    Dsymbol* s = dst->lookup(id);
    if (s)
    {
        if (s->isModule())
            mod = static_cast<Module*>(s);
        else
        {
            if (s->isAliasDeclaration())
            {
                ::error(loc, "%s `%s` conflicts with `%s`", s->kind(), s->toPrettyChars(), id->toChars());
            }
            else if (auto p = s->isPackage())
            {
                if (p->isPkgMod == PKGunknown)
                    mod = loadModule(sc, c_pparent);
                else
                    mod = p->isPackageMod();
                assert(mod && mod->isPackageFile == (p->isPkgMod == PKGmodule));
            }
        }
    }
    if (!mod)
    {
        // Load module
        mod = loadModule(sc, c_pparent);
        if (mod)
        {
            // id may be different from mod.ident, if so then insert alias
            dst->insert(id, mod);
        }
    }
    if (mod && !mod->importedFrom)
        mod->importedFrom = sc ? sc->_module->importedFrom : Module::rootModule;
    if (!pkg)
        pkg = mod;
    return global.errors != errors;
}

}
