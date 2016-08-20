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

using TemplateInstUnion = llvm::PointerUnion<clang::NamedDecl*, const clang::TemplateSpecializationType*>;

class TemplateDeclaration : public ::TemplateDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::NamedDecl *TempOrSpec;  // NOTE: we consider the primary template an explicit specialization as well

    TemplateDeclaration(Loc loc, Identifier *id, TemplateParameters *parameters,
                        Dsymbols *decldefs, const clang::NamedDecl *TempOrSpec);
    TemplateDeclaration(const TemplateDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
    bool checkTempDeclFwdRefs(Scope *sc, Dsymbol* tempdecl, ::TemplateInstance *ti) override;
    bool evaluateConstraint(::TemplateInstance *ti, Scope *sc, Scope *paramscope, Objects *dedtypes, ::FuncDeclaration *fd) override;
    bool earlyFunctionValidityCheck(::TemplateInstance *ti, Scope *sc, Objects *dedtypes) override;
    void prepareBestMatch(::TemplateInstance *ti, Scope *sc, Expressions *fargs) override;
    MATCH matchWithInstance(Scope *sc, ::TemplateInstance *ti, Objects *atypes, Expressions *fargs, int flag) override;
    MATCH leastAsSpecialized(Scope *sc, ::TemplateDeclaration *td2, Expressions *fargs) override;

    ::TemplateInstance *foreignInstance(::TemplateInstance *tithis, Scope *sc) override;
    void makeForeignInstance( cpp::TemplateInstance* ti );

    TemplateInstUnion hasExistingClangInst(::TemplateInstance* ti);
    TemplateInstUnion getClangInst(Scope* sc, ::TemplateInstance* ti, Objects* tdtypes = nullptr);
    clang::RedeclarableTemplateDecl *getPrimaryTemplate();
    TemplateDeclaration *primaryTemplate();
    static bool isForeignInstance(::TemplateInstance *ti);
    ::TemplateDeclaration *getCorrespondingTempDecl(TemplateInstUnion Inst);
    void correctTempDecl(TemplateInstance *ti);

    bool allowTupleParameterAnywhere() override { return true; }
};

class TemplateInstance : public ::TemplateInstance
{
public:
    CALYPSO_LANGPLUGIN

    bool isForeignInst = false;
    TemplateInstUnion Inst;
    Objects* primTiargs = nullptr;

    TemplateInstance(Loc loc, Identifier *temp_id);
    TemplateInstance(Loc loc, ::TemplateDeclaration *tempdecl, Objects *tiargs);
    TemplateInstance(const TemplateInstance&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
    Identifier *getIdent() override;
    bool semanticTiargs(Scope *sc) override;
    size_t correspondingParamIdx(size_t argi) override;
    TemplateParameter* correspondingParam(size_t argi) override;

    bool completeInst();
    void correctTiargs();

    const clang::NamedDecl* getInstantiatedTemplate();
};

}

#endif
