// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPDECLARATION_H
#define DMD_CPP_CPPDECLARATION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root/root.h"
#include "arraytypes.h"
#include "dsymbol.h"
#include "enum.h"
#include "cpp/calypso.h"
#include "cpp/cppexpression.h"
#include "cpp/cpptypes.h"

#include "../declaration.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "llvm/ADT/SmallPtrSet.h"

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
    unsigned offsetInBits; // for bit fields
    bool isUsed = false;

    VarDeclaration(Loc loc, Identifier *id,
                   const clang::ValueDecl *VD, Type *t, Initializer *init = nullptr);
    VarDeclaration(const VarDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool isOverlappedWith(::VarDeclaration *v2) override;
};

typedef llvm::SmallPtrSet<const clang::Decl*, 2> InstantiatedDeclSet;

class FuncDeclaration : public ::FuncDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::FunctionDecl *FD;
    bool isUsed = false;

    InstantiatedDeclSet instantiatedDecls;

    FuncDeclaration(Loc loc, Identifier *id, StorageClass storage_class,
                    Type* type, const clang::FunctionDecl *FD);
    FuncDeclaration(const FuncDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool functionSemantic3() override { return true; }
    bool allowFinalOverride() override { return true; }
    bool preferNonTemplateOverloads() override { return false; }
};

class CtorDeclaration : public ::CtorDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXConstructorDecl *CCD;
    bool isUsed = false;

    InstantiatedDeclSet instantiatedDecls;

    CtorDeclaration(Loc loc, StorageClass storage_class,
                    Type* type, const clang::CXXConstructorDecl *CCD);
    CtorDeclaration(const CtorDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool functionSemantic3() override { return true; }
    bool preferNonTemplateOverloads() override { return false; }
};

class DtorDeclaration : public ::DtorDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXDestructorDecl *CDD;
    bool isUsed = false;

    InstantiatedDeclSet instantiatedDecls;

    DtorDeclaration(Loc loc, StorageClass storage_class,
                    Identifier *id, const clang::CXXDestructorDecl *CDD);
    DtorDeclaration(const DtorDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool functionSemantic3() override { return true; }
    bool allowFinalOverride() override { return true; }
    bool preferNonTemplateOverloads() override { return false; }
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
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = IgnoreNone) override;
    void complete() override;

    Expression *getDefaultValue(const Loc &loc) override;
};

class EnumMember : public ::EnumMember
{
public:
    CALYPSO_LANGPLUGIN

    const clang::EnumConstantDecl *ECD;

    EnumMember(Loc loc, Identifier *id, Expression *value, Type *type,
               const clang::EnumConstantDecl *ECD);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
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
    Type *getType() override;
    Dsymbol *toAlias() override;
    Dsymbol *toAlias2() override;
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

// *************** //

// Run semantic() on referenced functions and record decls to instantiate templates and have them codegen'd
class DeclReferencer : public clang::RecursiveASTVisitor<DeclReferencer>
{
    ::FuncDeclaration* fdinst;

    Loc loc;

    bool Reference(const clang::NamedDecl *D);
    bool Reference(const clang::Type *T);

    bool VisitDeclRef(const clang::NamedDecl *D);

public:
    DeclReferencer(::FuncDeclaration* fdinst);

    void Traverse(const clang::FunctionDecl* D);

    bool VisitCXXConstructExpr(const clang::CXXConstructExpr *E);
    bool VisitCXXNewExpr(const clang::CXXNewExpr *E);
    bool VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E);
    bool VisitDeclRefExpr(const clang::DeclRefExpr *E);
    bool VisitMemberExpr(const clang::MemberExpr *E);

    bool shouldVisitImplicitCode() const { return true; }
};

inline bool& getIsUsed(::FuncDeclaration* fd)
{
    assert(isCPP(fd));

    if (fd->isCtorDeclaration())
        return static_cast<cpp::CtorDeclaration*>(fd)->isUsed;
    else if (fd->isDtorDeclaration())
        return static_cast<cpp::DtorDeclaration*>(fd)->isUsed;
    else
        return static_cast<cpp::FuncDeclaration*>(fd)->isUsed;
}

InstantiatedDeclSet& instantiatedDecls(Dsymbol* instantiatedBy);

}

#endif /* DMD_CPP_CPPDECLARATION_H */
