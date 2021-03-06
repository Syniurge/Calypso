
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/import.h
 */

#pragma once

#include "dsymbol.h"
#include "tokens.h"

class Identifier;
struct Scope;
class Module;
class Package;
class Catch;

class Import : public Dsymbol
{
public:
    /* static import aliasId = pkg1.pkg2.id : alias1 = name1, alias2 = name2;
     */

    Identifiers *packages;      // array of Identifier's representing packages
    Identifier *id;             // module Identifier
    Identifier *aliasId;
    int isstatic;               // !=0 if static import
    Prot protection;

    // Pairs of alias=name to bind into current namespace
    Identifiers names;
    Identifiers aliases;

    Module *mod;
    Package *pkg;               // leftmost package/module

    AliasDeclarations aliasdecls; // corresponding AliasDeclarations for alias=name pairs

    virtual void _key(); // CALYPSO
    void addAlias(Identifier *name, Identifier *alias); // CALYPSO
    const char *kind() const;
    Prot prot();
    Dsymbol *syntaxCopy(Dsymbol *s);    // copy only syntax trees
    virtual bool load(Scope *sc); // CALYPSO
    void importAll(Scope *sc);
    Dsymbol *toAlias();
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope* sc);
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);
    bool overloadInsert(Dsymbol *s);

    Import *isImport() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

struct InterState;
class ForeignCodeGen;

class LangPlugin
{
public:
    virtual void _init() = 0;

    // ===== - - - - - ===== //

    // returns -1 if said lang isn't handled by this plugin, or its id number
    // to be passed to createImport otherwise
    virtual int doesHandleImport(const char* lang) = 0;

    virtual Import *createImport(int langId,
        Loc loc, Identifiers *packages, Identifier *id,
        Identifier *aliasId, int isstatic) = 0;

    // ===== - - - - - ===== //

    virtual bool isForeignInstance(::TemplateInstance *ti) = 0;

    // ===== - - - - - ===== //

    virtual Type* typeForDTemplateArg(Type* t) = 0;

    // ===== - - - - - ===== //

    virtual int getPragma(Scope* sc, PragmaDeclaration* decl) = 0;
    virtual void pragmaSemantic(Scope* sc, PragmaDeclaration* decl) = 0;

    // ===== - - - - - ===== //

    virtual bool checkAccess(Loc loc, Scope* sc, Package* p) = 0;

    // ===== - - - - - ===== //

    // foreign exceptions
    virtual bool doesHandleCatch(LINK lang) = 0;
    virtual Catch *createCatch(Loc loc, Type *t, Identifier *id,
                               Statement *handler, StorageClass stc) = 0;

    // ===== - - - - - ===== //

    virtual Dsymbol *dispatchFuncCall(const Loc &loc, Scope *sc, Dsymbol *s,
                Objects *tiargs, Type *tthis, Expressions *fargs) = 0;

    virtual Expression *callCpCtor(Scope *sc, Expression *e) = 0;
    virtual Expression *constructCtorCall(const Loc& loc, Scope *sc, Expression *e1, Expression* e2) = 0;

    virtual bool mustCallerCallDtorOnArguments() = 0;

    virtual FuncDeclaration *searchOpEqualsForXopEquals(StructDeclaration *sd, Scope *sc) = 0;

    // ===== - - - - - ===== //

    virtual Expression* op_overload(Expression* e, Scope* sc, TOK* pop = nullptr) = 0;

    // ===== - - - - - ===== //

    virtual bool isSymbolReferenced(Dsymbol *s) { return true; }
    virtual void markSymbolReferenced(Dsymbol *s) = 0;

    // ===== - - - - - ===== //

    virtual Expression *semanticTraits(TraitsExp *e, Scope *sc) = 0;

    // ===== - - - - - ===== //

    virtual Expression *interpret(FuncDeclaration *fd, InterState *istate, Expressions *arguments,
                                  Expression *thisarg) = 0;
    virtual bool canInterpret(FuncDeclaration *fd) = 0;

    // ===== - - - - - ===== //

    virtual void adjustLinkerArgs(std::vector<std::string>& args) = 0;

    // ===== - - - - - ===== //

    // mars_mainBody hooks
    virtual void codegenModules() = 0;

    // ===== - - - - - ===== //

    virtual ForeignCodeGen *codegen() = 0;
    virtual bool needsCodegen(Module *m) = 0;
};

typedef Array<class LangPlugin *> LangPlugins;
extern LangPlugins langPlugins;
