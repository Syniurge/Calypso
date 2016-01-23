// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPAGGREGATE_H
#define DMD_CPP_CPPAGGREGATE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root.h"
#include "calypso.h"
#include "../aggregate.h"

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
    bool layoutQueried = false;

    StructDeclaration(Loc loc, Identifier* id, const clang::RecordDecl* RD);
    StructDeclaration(const StructDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void semantic(Scope *sc) override;
    unsigned size(Loc loc) override;
    Expression *defaultInit(Loc loc) override;
    bool mayBeAnonymous() override;
    bool disableDefaultCtor() override { return false; }
};

// The rest, i.e anything involving inheritance, virtual functions.
class ClassDeclaration : public ::ClassDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXRecordDecl *RD;
    bool layoutQueried = false;

    ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses,
                     const clang::CXXRecordDecl *RD);
    ClassDeclaration(const ClassDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void semantic(Scope *sc) override;
    bool mayBeAnonymous() override;
    unsigned size(Loc loc) override;
    
    bool isBaseOf(::ClassDeclaration* cd, int* poffset) override;
    void interfaceSemantic(Scope *sc) override;
    
    bool byRef() override { return false; }
    Expression *defaultInit(Loc loc) override;
    bool allowMultipleInheritance() override { return true; }
    bool allowInheritFromStruct() override { return true; }
    void makeNested() override;
    void finalizeVtbl() override;
    void buildLayout() override; // determine the agg size and field offsets

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
    unsigned size(Loc loc) override;
    bool mayBeAnonymous() override;
};

template<typename AggTy> void buildAggLayout(AggTy *ad);
const clang::RecordDecl *getRecordDecl(::AggregateDeclaration *ad);
const clang::RecordDecl *getRecordDecl(::Type *t);
::FuncDeclaration *findMethod(::AggregateDeclaration *ad, const clang::FunctionDecl *FD);

}

#endif /* DMD_CPP_CPPAGGREGATE_H */
