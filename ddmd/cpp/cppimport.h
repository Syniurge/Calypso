// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPIMPORT_H
#define DMD_CPP_CPPIMPORT_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "dsymbol.h"
#include "cpp/calypso.h"

#include "../import.h"

namespace cpp {

class Import final : public ::Import
{
public:
    CALYPSO_LANGPLUGIN

    Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId, int isstatic);

    ::Module *loadModule(Loc loc, Identifiers *packages, Identifier *id) override;
    void load(Scope *sc) override;
};

struct Modmap : public ::Modmap
{
public:
    CALYPSO_LANGPLUGIN

    Modmap(Loc loc, StringExp *arg);

    void importAll(Scope *sc) override;
    void semantic(Scope *sc) override;
};

// Special import for lookups from C++ modules
class GlobalImport : public ::Import
{
public:
    CALYPSO_LANGPLUGIN

    bool loaded = false;

    GlobalImport(Loc loc);
    void load(Scope *sc) override;
};

}

#endif /* DMD_CPP_CPPIMPORT_H */
