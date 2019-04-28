// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPAGGREGATE_H
#define DMD_CPP_CPPAGGREGATE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root/root.h"
#include "cpp/calypso.h"
#include "aggregate.h"
#include "attrib.h"

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
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    bool determineFields() override;
    bool buildLayout() override;
    void finalizeSize() override;
    Expression *defaultInit(Loc loc) override;
    bool mayBeAnonymous() override;
    bool isBaseOf(::ClassDeclaration* cd, int* poffset) override;
    bool disableDefaultCtor() override { return false; }
    ::CtorDeclaration* hasCopyCtor(Scope* sc) override;
    Expression* buildVarInitializer(Scope* sc, ::VarDeclaration* vd, Expression* exp) override;

    void accept(Visitor *v) override;
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
                     Dsymbols* members, const clang::CXXRecordDecl *RD);
    ClassDeclaration(const ClassDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    bool determineFields() override;
    bool buildLayout() override;
    bool mayBeAnonymous() override;

    void addLocalClass(ClassDeclarations *) override;

    bool isBaseOf(::ClassDeclaration* cd, int* poffset) override;

    bool byRef() const override { return false; }
    Expression *defaultInit(Loc loc) override;
    bool allowMultipleInheritance() override { return true; }
    bool allowInheritFromStruct() override { return true; }
    bool needsInterfaceSemantic() const override { return false; }
    void makeNested() override;
    void finalizeVtbl() override;
    ::CtorDeclaration* hasCopyCtor(Scope* sc) override;
    Expression* buildVarInitializer(Scope* sc, ::VarDeclaration* vd, Expression* exp) override;

    void accept(Visitor *v) override;
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
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    bool mayBeAnonymous() override;
    bool determineFields() override;
    bool buildLayout() override;
    void finalizeSize() override;
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

void MarkAggregateReferencedImpl(AggregateDeclaration* ad);

#endif /* DMD_CPP_CPPAGGREGATE_H */
