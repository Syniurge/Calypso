
/* Compiler implementation of the D programming language
 * Copyright (c) 1999-2016 by Digital Mars
 * All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/import.h
 */

#ifndef DMD_IMPORT_H
#define DMD_IMPORT_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "dsymbol.h"


class Identifier;
struct Scope;
class Module;
class Package;
class AliasDeclaration;
class StringExp;
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
    void setSymIdent(); // CALYPSO
    void addAlias(Identifier *name, Identifier *alias);
    const char *kind() const;
    Prot prot();
    Dsymbol *syntaxCopy(Dsymbol *s);    // copy only syntax trees
    virtual void load(Scope *sc); // CALYPSO
    void importAll(Scope *sc);
    void semantic(Scope *sc);
    Dsymbol *toAlias();
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope* sc);
    Dsymbol *search(Loc loc, Identifier *ident, int flags = SearchLocalsOnly);
    bool overloadInsert(Dsymbol *s);

    // CALYPSO
    virtual Module *loadModule(Scope* sc);

    Import *isImport() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class Modmap : public Dsymbol
{
public:
    StringExp *arg;

//     Modmap(Loc loc, StringExp *arg);
    virtual void _key(); // CALYPSO
    void setScope(Scope* sc);

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
    virtual int doesHandleModmap(const char* lang) = 0;

    virtual Modmap *createModmap(int langId,
        Loc loc, Expression *arg) = 0;

    // returns -1 if said tree isn't handled by this plugin, or its id number
    // to be passed to createImport otherwise
    virtual int doesHandleImport(const char* tree) = 0;

    virtual Import *createImport(int treeId,
        Loc loc, Identifiers *packages, Identifier *id,
        Identifier *aliasId, int isstatic) = 0;

    // foreign exceptions
    virtual bool doesHandleCatch(LINK lang) = 0;
    virtual Catch *createCatch(Loc loc, Type *t, Identifier *id,
                               Statement *handler, StorageClass stc) = 0;

    // ===== - - - - - ===== //

    virtual const char *mangle(Dsymbol *s) = 0; // TODO replace by getForeignMangler
    virtual void mangleAnonymousAggregate(OutBuffer *buf, AggregateDeclaration* ad) = 0; // HACK

    // create a mangler for types and symbols specific to this plugin
    // base is the D mangler
    virtual Visitor *getForeignMangler(OutBuffer *buf, bool forEquiv, Visitor *base) = 0;

    // ===== - - - - - ===== //

    virtual Expression *getRightThis(Loc loc, Scope *sc, AggregateDeclaration *ad,
        Expression *e1, Declaration *var, int flag = 0) = 0;
    virtual Expression *callCpCtor(Scope *sc, Expression *e) = 0;

    virtual FuncDeclaration *buildDtor(AggregateDeclaration *ad, Scope *sc) = 0;
    virtual FuncDeclaration *buildOpAssign(StructDeclaration *sd, Scope *sc) = 0;
    virtual FuncDeclaration *searchOpEqualsForXopEquals(StructDeclaration *sd, Scope *sc) = 0;

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

    virtual void semanticModules() = 0;
    virtual void codegenModules() = 0;

    // ===== - - - - - ===== //

    virtual ForeignCodeGen *codegen() = 0;
    virtual bool needsCodegen(Module *m) = 0;
};

typedef Array<class LangPlugin *> LangPlugins;
extern LangPlugins langPlugins;

#endif /* DMD_IMPORT_H */
