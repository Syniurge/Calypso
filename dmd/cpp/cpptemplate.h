// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPTEMPLATE_H
#define DMD_CPP_CPPTEMPLATE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root/root.h"
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

    const clang::NamedDecl *TempOrSpec;

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
    Dsymbols* copySyntaxTree(::TemplateInstance *ti) override;
    MATCH deduceFunctionTemplateMatch(::TemplateInstance *ti, Scope *sc, ::FuncDeclaration *&fd, Type *tthis, Expressions *fargs) override;

    MATCH functionTemplateMatch(::TemplateInstance *ti, Expressions *fargs, TemplateInstUnion& Inst);

    Objects* tdtypesFromInst(TemplateInstUnion Inst, bool forForeignInstance = false);

    ::TemplateInstance *foreignInstance(::TemplateInstance *tithis, Scope *sc) override;
    void makeForeignInstance(TemplateInstance* ti, Scope* sc);

    TemplateInstUnion hasExistingClangInst(::TemplateInstance* ti);
    TemplateInstUnion getClangInst(Scope* sc, ::TemplateInstance* ti, Objects* tdtypes = nullptr);
    clang::RedeclarableTemplateDecl *getPrimaryTemplate();
    TemplateDeclaration *primaryTemplate();
    static bool isForeignInstance(::TemplateInstance *ti);
    ::TemplateDeclaration *getCorrespondingTempDecl(TemplateInstUnion Inst);
    void correctTempDecl(TemplateInstance *ti);

    bool allowTupleParameterAnywhere() override { return true; }

    void accept(Visitor *v) override;
};

class TemplateInstance : public ::TemplateInstance
{
public:
    CALYPSO_LANGPLUGIN

    bool isForeignInst = false;
    TemplateInstUnion Inst;
    Objects* primTiargs = nullptr;

    TemplateInstance(Loc loc, Identifier *ident, Objects *tiargs);
    TemplateInstance(Loc loc, ::TemplateDeclaration *td, Objects *tiargs);
    TemplateInstance(const TemplateInstance&);
    Dsymbol *syntaxCopy(Dsymbol *) override;
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    Identifier *getIdent() override;
    bool semanticTiargs(Scope *sc) override;
    void declareParameters(Scope *sc) override {}
    size_t correspondingParamIdx(size_t argi) override;
    TemplateParameter* correspondingParam(size_t argi) override;
    Objects* tiargsForMangling() override;

    bool completeInst();
    void correctTiargs(Scope* sc);
    void markInvalid();

    const clang::NamedDecl* getInstantiatedTemplate();

    // Debug methods
    clang::NamedDecl* getInstND();
    const clang::TemplateSpecializationType* getInstTST();
};

}

#endif
