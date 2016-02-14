// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPTEMPLATE_H
#define DMD_CPP_CPPTEMPLATE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "cpp/calypso.h"
#include "template.h"

namespace clang
{
class Decl;
}

namespace cpp
{
class TemplateInstance;

class TemplateDeclaration : public ::TemplateDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::NamedDecl *TempOrSpec;  // NOTE: we consider the primary template an explicit specialization as well

    TemplateDeclaration(Loc loc, Identifier *id, TemplateParameters *parameters,
                        Dsymbols *decldefs, const clang::NamedDecl *TempOrSpec);
    TemplateDeclaration(const TemplateDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
    bool evaluateConstraint(::TemplateInstance *ti, Scope *sc, Scope *paramscope, Objects *dedtypes, ::FuncDeclaration *fd) override;
    MATCH matchWithInstance(Scope *sc, ::TemplateInstance *ti, Objects *atypes, Expressions *fargs, int flag) override;
    ::TemplateInstance *foreignInstance(::TemplateInstance *tithis, Scope *sc) override;

    clang::RedeclarableTemplateDecl *getPrimaryTemplate();
    static bool isForeignInstance(::TemplateInstance *ti);
    void correctTempDecl(TemplateInstance *ti);
};

class TemplateInstance : public ::TemplateInstance
{
public:
    CALYPSO_LANGPLUGIN

    clang::Decl *Inst = nullptr;
    llvm::SmallVector<clang::Decl*, 4> Dependencies; // Functions external to the template but instantiated during completeInst(), since they weren't instantiated before we need to emit them as well

    Objects *origTiargs = nullptr; // needed for deco mangling

    TemplateInstance(Loc loc, Identifier *temp_id);
    TemplateInstance(Loc loc, ::TemplateDeclaration *tempdecl, Objects *tiargs);
    TemplateInstance(const TemplateInstance&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
    Identifier *getIdent() override;

    bool completeInst();
    void correctTiargs();
};

}

#endif
