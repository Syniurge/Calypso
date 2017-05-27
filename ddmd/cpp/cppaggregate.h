// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPAGGREGATE_H
#define DMD_CPP_CPPAGGREGATE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "calypso.h"
#include "../aggregate.h"
#include "../attrib.h"

namespace clang
{
class RecordDecl;
class CXXRecordDecl; // NOTE: will disappear in a future version of Clang
struct ThunkInfo;
}

namespace cpp
{
class FuncDeclaration;

// All non-polymorphic C++ aggregate types, it doesn't matter whether "struct" or "class"
// was used and whether another aggregate inherit from it
class StructDeclaration : public ::StructDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::RecordDecl *RD;
    bool isUsed = false;
    bool layoutQueried = false;

    StructDeclaration(Loc loc, Identifier* id, const clang::RecordDecl* RD);
    StructDeclaration(const StructDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void semantic(Scope *sc) override;
    void semantic3(Scope *sc) override;
    void buildLayout() override;
    void finalizeSize() override;
    Expression *defaultInit(Loc loc) override;
    bool mayBeAnonymous() override;
    bool isBaseOf(::ClassDeclaration* cd, int* poffset) override;
    bool disableDefaultCtor() override { return false; }
    Expression* buildVarInitializer(Scope* sc, ::VarDeclaration* vd, Expression* exp) override;
};

// The rest, i.e anything involving inheritance, virtual functions.
class ClassDeclaration : public ::ClassDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXRecordDecl *RD;
    bool isUsed = false;
    bool layoutQueried = false;

    ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses,
                     const clang::CXXRecordDecl *RD);
    ClassDeclaration(const ClassDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void semantic(Scope *sc) override;
    void semantic3(Scope *sc) override;
    void buildLayout() override;
    bool mayBeAnonymous() override;

    void addLocalClass(ClassDeclarations *) override;

    bool isBaseOf(::ClassDeclaration* cd, int* poffset) override;
    void interfaceSemantic(Scope *sc) override;

    bool byRef() const override { return false; }
    Expression *defaultInit(Loc loc) override;
    bool allowMultipleInheritance() override { return true; }
    bool allowInheritFromStruct() override { return true; }
    void makeNested() override;
    void finalizeVtbl() override;
    Expression* buildVarInitializer(Scope* sc, ::VarDeclaration* vd, Expression* exp) override;

    void buildCpCtor(Scope *sc);
};

class UnionDeclaration : public ::UnionDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::RecordDecl *RD;
    bool layoutQueried = false;

    UnionDeclaration(Loc loc, Identifier* id, const clang::RecordDecl* RD);
    UnionDeclaration(const UnionDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool mayBeAnonymous() override;
    void buildLayout() override;
};

class AnonDeclaration : public ::AnonDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::FieldDecl *AnonField = nullptr; // corresponding anonymous field in the parent record if any

    AnonDeclaration(Loc loc, bool isunion, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
};

const clang::RecordDecl *getRecordDecl(::AggregateDeclaration *ad);
const clang::RecordDecl *getRecordDecl(::Type *t);
::FuncDeclaration *findMethod(::AggregateDeclaration *ad, const clang::FunctionDecl *FD);
::FuncDeclaration *findOverriddenMethod(::FuncDeclaration* md, ::ClassDeclaration* base );

}

#endif /* DMD_CPP_CPPAGGREGATE_H */
