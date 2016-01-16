// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPDECLARATION_H
#define DMD_CPP_CPPDECLARATION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "dsymbol.h"
#include "enum.h"
#include "cpp/calypso.h"
#include "cpp/cpptypes.h"

#include "../declaration.h"

#include "clang/AST/RecursiveASTVisitor.h"

namespace clang
{
class ValueDecl;
class FunctionProtoType;
class FunctionDecl;
class CXXConstructorDecl;
class CXXDestructorDecl;
}

namespace cpp
{

class VarDeclaration : public ::VarDeclaration
{
public:
    CALYPSO_LANGPLUGIN
    
    const clang::ValueDecl *VD;

    VarDeclaration(Loc loc, Identifier *id,
                   const clang::ValueDecl *VD, Type *t, Initializer *init = nullptr);
    VarDeclaration(const VarDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
};

class FuncDeclaration : public ::FuncDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::FunctionDecl *FD;

    FuncDeclaration(Loc loc, Identifier *id, StorageClass storage_class,
                    Type* type, const clang::FunctionDecl *FD);
    FuncDeclaration(const FuncDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void semantic(Scope *sc) override;
    void semantic3(Scope *sc) override;
    bool functionSemantic3() override { return true; }
    bool allowFinalOverride() override { return true; }

    static void cppSemantic(::FuncDeclaration *fd, Scope *sc);
    static void semantic3reference(::FuncDeclaration *fd, Scope *sc);
    static ::FuncDeclaration *overloadCppMatch(::FuncDeclaration *fd, const clang::FunctionDecl* FD);
};

class CtorDeclaration : public ::CtorDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXConstructorDecl *CCD;

    CtorDeclaration(Loc loc, StorageClass storage_class,
                    Type* type, const clang::CXXConstructorDecl *CCD);
    CtorDeclaration(const CtorDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void semantic(Scope *sc) override;
    void semantic3(Scope *sc) override;
    bool functionSemantic3() override { return true; }
};

class DtorDeclaration : public ::DtorDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXDestructorDecl *CDD;

    DtorDeclaration(Loc loc, StorageClass storage_class,
                    Identifier *id, const clang::CXXDestructorDecl *CDD);
    DtorDeclaration(const DtorDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void semantic(Scope *sc) override;
    void semantic3(Scope *sc) override;
    bool functionSemantic3() override { return true; }
    bool allowFinalOverride() override { return true; }
};

class EnumDeclaration : public ::EnumDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::EnumDecl *ED;

    EnumDeclaration(Loc loc, Identifier *id, Type *memtype,
            const clang::EnumDecl *ED);
    EnumDeclaration(const EnumDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
};

class AliasDeclaration : public ::AliasDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::TypedefNameDecl *TND;

    AliasDeclaration(Loc loc, Identifier *ident, Type *type,
            const clang::TypedefNameDecl *TND);
    AliasDeclaration(const AliasDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
};

// We need aliases for overloaded operators, but aliases do not reference a specific function and then resolving the operator call fails if there are more than one match
// We can't set the aliassym straightaway because the parents need to be semantic'd before semantic'ing the referenced function
// Maybe there's a more elegant and shorter solution but for now that'll do
class OverloadAliasDeclaration : public ::AliasDeclaration
{
public:
    TypeFunction *overtf;

    OverloadAliasDeclaration(Loc loc, Identifier *ident, Type *type,
                             TypeFunction *overtf);
    OverloadAliasDeclaration(const OverloadAliasDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void semantic(Scope *sc) override;
    ::AliasDeclaration *isOverloadAliasDeclaration() override { return this; }
};

const clang::FunctionDecl *getFD(::FuncDeclaration *f);

#define IMPLEMENT_syntaxCopy(Class, D) \
    Dsymbol* Class::syntaxCopy(Dsymbol* s) \
    { \
        if (!s) \
            s = new cpp::Class(*this); \
        assert(isCPP(s) && s->is##Class()); \
        static_cast<cpp::Class*>(s)->D = D; \
        return ::Class::syntaxCopy(s); \
    }
// NOTE: we use copy constructors only to copy the arguments passed to the main constructor, the rest is handled by syntaxCopy

bool isMapped(const clang::Decl *D);

class DeclMapper : public TypeMapper
{
public:
    DeclMapper(Module *mod)
        : TypeMapper(mod) {} // hmm why does putting into .cpp give me a link error?

    inline Prot toProt(clang::AccessSpecifier AS);

    // Declarations
    Dsymbols *VisitDeclContext(const clang::DeclContext *DC);

    Dsymbols *VisitDecl(const clang::Decl *D, unsigned flags = 0);

    Dsymbols *VisitValueDecl(const clang::ValueDecl *D);
    Dsymbols *VisitRecordDecl(const clang::RecordDecl* D, unsigned flags = 0);
    Dsymbols *VisitTypedefNameDecl(const clang::TypedefNameDecl *D);
    Dsymbols *VisitFunctionDecl(const clang::FunctionDecl *D, unsigned flags = 0);
    Dsymbols *VisitRedeclarableTemplateDecl(const clang::RedeclarableTemplateDecl* D);
    Dsymbols *VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D);
    Dsymbols *VisitEnumDecl(const clang::EnumDecl *D);

    Dsymbol *VisitInstancedClassTemplate(const clang::ClassTemplateSpecializationDecl *D, unsigned int flags = 0); // entry point when mapping instances during semantic()
    ::FuncDeclaration *VisitInstancedFunctionTemplate(const clang::FunctionDecl *D);
    TemplateParameter *VisitTemplateParameter(const clang::NamedDecl *Param,
                                                                    const clang::TemplateArgument *SpecArg = nullptr); // in DMD explicit specializations use parameters, whereas Clang uses args

    static const unsigned ForcePolymorphic = 1 << 0; // When a templace declaration is polymorphic, we want the explicit template specializations to be polymorphic too even if isPolymorphic() is false
    static const unsigned MapImplicitRecords = 1 << 1;
    static const unsigned MapTemplateInstantiations = 1 << 2;
    static const unsigned MapExplicitSpecs = 1 << 3; // If not set explicit and partial specs will be discarded by VisitDecl
    static const unsigned NamedValueWithAnonRecord = 1 << 4; // Only set when called from VisitValueDecl for e.g union {...} myUnion

    static Identifier *getIdentifierForTemplateNonTypeParm(const clang::NonTypeTemplateParmDecl *NTTPD);
};

// Run semantic() on referenced functions and record decls to instantiate templates and have them codegen'd
class DeclReferencer : public clang::RecursiveASTVisitor<DeclReferencer>
{
    TypeMapper mapper;
    Loc loc;
    Scope *sc = nullptr;
    TypeQualifiedBuilderOptions tqualOptions;

    llvm::DenseSet<const clang::Decl *> Referenced;

    bool Reference(const clang::NamedDecl *D);
    bool Reference(const clang::Type *T);
    void ReferenceTemplateArguments(const clang::NamedDecl *D);

    bool VisitDeclRef(const clang::NamedDecl *D);
public:
    DeclReferencer()
    {
        mapper.addImplicitDecls = false;
        mapper.cppPrefix = false;
        tqualOptions.overOpSkipSpecArg = true;
    }

    void Traverse(Loc loc, Scope *sc, clang::Stmt *S);

    bool VisitCXXConstructExpr(const clang::CXXConstructExpr *E);
    bool VisitCXXNewExpr(const clang::CXXNewExpr *E);
    bool VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E);
    bool VisitDeclRefExpr(const clang::DeclRefExpr *E);
    bool VisitMemberExpr(const clang::MemberExpr *E);
};

extern DeclReferencer declReferencer;
Scope *globalScope(::Module *m);

const clang::Decl *getCanonicalDecl(const clang::Decl *D); // the only difference with D->getCanonicalDecl() is that if the canonical decl is an out-of-ilne friend' decl and the actual decl is declared, this returns the latter instead of the former

}

#endif /* DMD_CPP_CPPDECLARATION_H */
