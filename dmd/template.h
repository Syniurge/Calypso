
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/template.h
 */

#pragma once

#include "arraytypes.h"
#include "dsymbol.h"

class Identifier;
class TemplateInstance;
class TemplateParameter;
class TemplateTypeParameter;
class TemplateThisParameter;
class TemplateValueParameter;
class TemplateAliasParameter;
class TemplateTupleParameter;
class Type;
class TypeQualified;
struct Scope;
class Expression;
class FuncDeclaration;
class Parameter;

class Tuple : public RootObject
{
public:
    Objects objects;

    // kludge for template.isType()
    DYNCAST dyncast() const { return DYNCAST_TUPLE; }

    const char *toChars() { return objects.toChars(); }
};

struct TemplatePrevious
{
    TemplatePrevious *prev;
    Scope *sc;
    Objects *dedargs;
};

class TemplateDeclaration : public ScopeDsymbol
{
public:
    TemplateParameters *parameters;     // array of TemplateParameter's

    TemplateParameters *origParameters; // originals for Ddoc
    Expression *constraint;

    // Hash table to look up TemplateInstance's of this TemplateDeclaration
    void *instances;

    TemplateDeclaration *overnext;      // next overloaded TemplateDeclaration
    TemplateDeclaration *overroot;      // first in overnext list
    FuncDeclaration *funcroot;          // first function in unified overload list

    Dsymbol *onemember;         // if !=NULL then one member of this template

    bool literal;               // this template declaration is a literal
    bool ismixin;               // template declaration is only to be used as a mixin
    bool isstatic;              // this is static template declaration
    Prot protection;
    int inuse;                  // for recursive expansion detection

    TemplatePrevious *previous;         // threaded list of previous instantiation attempts on stack

#if IN_LLVM
    const char *intrinsicName;
#endif

private:
    Expression* lastConstraint; /// the constraint after the last failed evaluation
    Expressions lastConstraintNegs; /// its negative parts
    Objects* lastConstraintTiargs; /// template instance arguments for `lastConstraint`

public:
    virtual void _key(); // CALYPSO
    Dsymbol *syntaxCopy(Dsymbol *);
    bool overloadInsert(Dsymbol *s);
    bool hasStaticCtorOrDtor();
    const char *kind() const;
    const char *toChars();

    Prot prot();

    virtual bool evaluateConstraint(TemplateInstance *ti, Scope *sc, Scope *paramscope, Objects *dedtypes, FuncDeclaration *fd); // CALYPSO

    virtual bool earlyFunctionValidityCheck(TemplateInstance *ti, Scope *sc, Objects *dedtypes) { return true; } // CALYPSO
    virtual void prepareBestMatch(TemplateInstance* ti, Scope* sc, Expressions* fargs); // CALYPSO

    virtual MATCH matchWithInstance(Scope *sc, TemplateInstance *ti, Objects *atypes, Expressions *fargs, int flag); // CALYPSO
    virtual MATCH leastAsSpecialized(Scope *sc, TemplateDeclaration *td2, Expressions *fargs); // CALYPSO
    virtual Dsymbols* copySyntaxTree(TemplateInstance *ti); // CALYPSO TODO 1.1: move below?

    virtual MATCH deduceFunctionTemplateMatch(TemplateInstance *ti, Scope *sc, FuncDeclaration *&fd, Type *tthis, Expressions *fargs); // CALYPSO
    RootObject *declareParameter(Scope *sc, TemplateParameter *tp, RootObject *o);
    FuncDeclaration *doHeaderInstantiation(TemplateInstance *ti, Scope *sc, FuncDeclaration *fd, Type *tthis, Expressions *fargs); // CALYPSO

    // CALYPSO (made extern (C++))
    TemplateInstance* findExistingInstance(TemplateInstance* tithis, Expressions* fargs);
    TemplateInstance* addInstance(TemplateInstance* ti);

    // CALYPSO
    virtual TemplateInstance *foreignInstance(TemplateInstance *tithis, Scope *sc) { return NULL; }

    // CALYPSO variadic template generalization (pretty intrusive...)
    size_t numParameterPacks();
    virtual bool allowTupleParameterAnywhere() { return false; }

    TemplateDeclaration *isTemplateDeclaration() { return this; }

    TemplateTupleParameter *isVariadic();
    bool isOverloadable() const;

    void accept(Visitor *v) { v->visit(this); }
};

/* For type-parameter:
 *  template Foo(ident)             // specType is set to NULL
 *  template Foo(ident : specType)
 * For value-parameter:
 *  template Foo(valType ident)     // specValue is set to NULL
 *  template Foo(valType ident : specValue)
 * For alias-parameter:
 *  template Foo(alias ident)
 * For this-parameter:
 *  template Foo(this ident)
 */
class TemplateParameter : public ASTNode
{
public:
    Loc loc;
    Identifier *ident;

    /* True if this is a part of precedent parameter specialization pattern.
     *
     *  template A(T : X!TL, alias X, TL...) {}
     *  // X and TL are dependent template parameter
     *
     * A dependent template parameter should return MATCHexact in matchArg()
     * to respect the match level of the corresponding precedent parameter.
     */
    bool dependent;

    virtual TemplateTypeParameter  *isTemplateTypeParameter();
    virtual TemplateValueParameter *isTemplateValueParameter();
    virtual TemplateAliasParameter *isTemplateAliasParameter();
    virtual TemplateThisParameter *isTemplateThisParameter();
    virtual TemplateTupleParameter *isTemplateTupleParameter();

    virtual TemplateParameter *syntaxCopy() = 0;
    virtual bool declareParameter(Scope *sc) = 0;
    virtual void print(RootObject *oarg, RootObject *oded) = 0;
    virtual RootObject *specialization() = 0;
    virtual RootObject *defaultArg(Loc instLoc, Scope *sc) = 0;
    virtual bool hasDefaultArg() = 0;

    /* Create dummy argument based on parameter.
     */
    virtual void *dummyArg() = 0;
    void accept(Visitor *v) { v->visit(this); }
};

/* Syntax:
 *  ident : specType = defaultType
 */
class TemplateTypeParameter : public TemplateParameter
{
public:
    Type *specType;     // type parameter: if !=NULL, this is the type specialization
    Type *defaultType;

    TemplateTypeParameter *isTemplateTypeParameter();
    TemplateParameter *syntaxCopy();
    bool declareParameter(Scope *sc);
    void print(RootObject *oarg, RootObject *oded);
    RootObject *specialization();
    RootObject *defaultArg(Loc instLoc, Scope *sc);
    bool hasDefaultArg();
    void *dummyArg();
    void accept(Visitor *v) { v->visit(this); }
};

/* Syntax:
 *  this ident : specType = defaultType
 */
class TemplateThisParameter : public TemplateTypeParameter
{
public:
    TemplateThisParameter *isTemplateThisParameter();
    TemplateParameter *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

/* Syntax:
 *  valType ident : specValue = defaultValue
 */
class TemplateValueParameter : public TemplateParameter
{
public:
    Type *valType;
    Expression *specValue;
    Expression *defaultValue;

    TemplateValueParameter *isTemplateValueParameter();
    TemplateParameter *syntaxCopy();
    bool declareParameter(Scope *sc);
    void print(RootObject *oarg, RootObject *oded);
    RootObject *specialization();
    RootObject *defaultArg(Loc instLoc, Scope *sc);
    bool hasDefaultArg();
    void *dummyArg();
    void accept(Visitor *v) { v->visit(this); }
};

/* Syntax:
 *  specType ident : specAlias = defaultAlias
 */
class TemplateAliasParameter : public TemplateParameter
{
public:
    Type *specType;
    RootObject *specAlias;
    RootObject *defaultAlias;

    TemplateAliasParameter *isTemplateAliasParameter();
    TemplateParameter *syntaxCopy();
    bool declareParameter(Scope *sc);
    void print(RootObject *oarg, RootObject *oded);
    RootObject *specialization();
    RootObject *defaultArg(Loc instLoc, Scope *sc);
    bool hasDefaultArg();
    void *dummyArg();
    void accept(Visitor *v) { v->visit(this); }
};

/* Syntax:
 *  ident ...
 */
class TemplateTupleParameter : public TemplateParameter
{
public:
    TemplateTupleParameter *isTemplateTupleParameter();
    TemplateParameter *syntaxCopy();
    bool declareParameter(Scope *sc);
    void print(RootObject *oarg, RootObject *oded);
    RootObject *specialization();
    RootObject *defaultArg(Loc instLoc, Scope *sc);
    bool hasDefaultArg();
    void *dummyArg();
    void accept(Visitor *v) { v->visit(this); }
};

/* Given:
 *  foo!(args) =>
 *      name = foo
 *      tiargs = args
 */
class TemplateInstance : public ScopeDsymbol
{
public:
    Identifier *name;

    // Array of Types/Expressions of template
    // instance arguments [int*, char, 10*10]
    Objects *tiargs;

    // Array of Types/Expressions corresponding
    // to TemplateDeclaration.parameters
    // [int, char, 100]
    Objects tdtypes;

    // Modules imported by this template instance
    Modules importedModules;

    Dsymbol *tempdecl;                  // referenced by foo.bar.abc
    Dsymbol *enclosing;                 // if referencing local symbols, this is the context
    Dsymbol *aliasdecl;                 // !=NULL if instance is an alias for its sole member
    TemplateInstance *inst;             // refer to existing instance
    ScopeDsymbol *argsym;               // argument symbol table
    int inuse;                          // for recursive expansion detection
    int nest;                           // for recursive pretty printing detection
    bool semantictiargsdone;            // has semanticTiargs() been done?
    bool havetempdecl;                  // if used second constructor
    bool gagged;                        // if the instantiation is done with error gagging
    hash_t hash;                        // cached result of toHash()
    Expressions *fargs;                 // for function template, these are the function arguments

    TemplateInstances* deferred;

    Module *memberOf;                   // if !null, then this TemplateInstance appears in memberOf.members[]

    // Used to determine the instance needs code generation.
    // Note that these are inaccurate until semantic analysis phase completed.
    TemplateInstance *tinst;            // enclosing template instance
    TemplateInstance *tnext;            // non-first instantiated instances
    Module *minst;                      // the top module that instantiated this instance

    bool isDummy;                       // if true, this TemplateInstance may be allocated on stack (CALYPSO DMD BUG FIX)

    virtual void _key(); // CALYPSO
    Dsymbol *syntaxCopy(Dsymbol *);
    Dsymbol *toAlias();                 // resolve real symbol
    const char *kind() const;
    bool oneMember(Dsymbol **ps, Identifier *ident);
    const char *toChars();
    const char* toPrettyCharsHelper();
    void printInstantiationTrace();
    virtual Identifier *getIdent(); // CALYPSO made virtual to use the original tiargs for mangling
    hash_t toHash();

    bool needsCodegen();

    TemplateDeclaration* firstTempDecl(); // CALYPSO returns the first overload
    static bool semanticTiargs(const Loc &loc, Scope *sc, Objects *tiargs, int flags); // CALYPSO
    virtual bool semanticTiargs(Scope *sc); // CALYPSO
    virtual Dsymbols* appendToModuleMember();
    virtual void declareParameters(Scope *sc); // CALYPSO

    // CALYPSO variadic template generalization (pretty intrusive...)
    virtual size_t correspondingParamIdx(size_t argi);
    virtual TemplateParameter* correspondingParam(size_t argi);
    virtual Objects* tiargsForMangling() { return tiargs; }

    TemplateInstance *isTemplateInstance() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class TemplateMixin : public TemplateInstance
{
public:
    TypeQualified *tqual;

    Dsymbol *syntaxCopy(Dsymbol *s);
    const char *kind() const;
    bool oneMember(Dsymbol **ps, Identifier *ident);
    int apply(Dsymbol_apply_ft_t fp, void *param);
    bool hasPointers();
    void setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion);
    const char *toChars();

    TemplateMixin *isTemplateMixin() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

Expression *isExpression(RootObject *o);
Dsymbol *isDsymbol(RootObject *o);
Type *isType(RootObject *o);
Tuple *isTuple(RootObject *o);
Parameter *isParameter(RootObject *o);
TemplateParameter *isTemplateParameter(RootObject *o);
bool arrayObjectIsError(const Objects *args);
bool isError(const RootObject *const o);
Type *getType(RootObject *o);
Dsymbol *getDsymbol(RootObject *o);

RootObject *objectSyntaxCopy(RootObject *o);

// CALYPSO
MATCH matchArg(TemplateParameter* tp, Loc instLoc, Scope *sc, Objects *tiargs, size_t prmi, size_t* argi /*CALYPSO*/, TemplateParameters *parameters, Objects *dedtypes, Declaration **psparam);
// MATCH matchArg(Scope *sc, RootObject *oarg, size_t i, TemplateParameters *parameters, Objects *dedtypes, Declaration **psparam);
