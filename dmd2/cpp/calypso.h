// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CALYPSO_H
#define DMD_CPP_CALYPSO_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "../mars.h"
#include "../import.h"
#include "../gen/cgforeign.h"

#include <memory>
#include "llvm/ADT/StringSet.h"
#include "llvm/IR/DataLayout.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "clang/lib/CodeGen/CodeGenModule.h"

#define USE_CLANG_MODULES

class Identifier;

namespace clang
{
class IdentifierInfo;
class CodeGenFunction;
class Sema;
class ASTUnit;
class MacroInfo;
}

namespace cpp
{

class ClassDeclaration;
class BuiltinTypes;
class TemplateInstance;
class TypeMapper;
class DeclReferencer;

namespace reclang { class ModuleMap; class DiagnosticPrinter; }
using clang::ASTUnit;
using reclang::ModuleMap;
using reclang::DiagnosticPrinter;

namespace clangCG = clang::CodeGen;

struct SpecValue
{
    const char *op = nullptr; // for overloaded operators
    Type *t = nullptr; // for conversion operators

    TypeMapper &mapper;
    SpecValue(TypeMapper &mapper) : mapper(mapper) {}

    operator bool() { return op || t; }
    RootObject *toTemplateArg(Loc loc);
};

Identifier *fromIdentifier(const clang::IdentifierInfo *II);
Identifier *fromDeclarationName(const clang::DeclarationName N, SpecValue *spec = nullptr);
Identifier *getIdentifier(const clang::NamedDecl* D, cpp::SpecValue* spec = nullptr, bool useCanonicalType = false);
Identifier *getIdentifierOrNull(const clang::NamedDecl* D, cpp::SpecValue* spec = nullptr, bool useCanonicalType = false);
Identifier *getExtendedIdentifier(const clang::NamedDecl *D, TypeMapper &mapper); // will return the name of the non-templated method for operators, same than getIdentifier() for other Decls
Identifier *getExtendedIdentifierOrNull(const clang::NamedDecl *D, TypeMapper &mapper);

RootObject *getIdentOrTempinst(Loc loc, const clang::DeclarationName N,
                               TypeMapper &mapper);

const clang::TagDecl *isOverloadedOperatorWithTagOperand(const clang::Decl *D,
                                                           const clang::NamedDecl *SpecificTag = nullptr);

Loc fromLoc(clang::SourceLocation L);

class InstantiationChecker : public clang::ASTConsumer, public clang::ASTMutationListener
{
public:
    clang::ASTMutationListener *GetASTMutationListener() override { return this; }

    void CompletedImplicitDefinition(const clang::FunctionDecl *D) override;
    void FunctionDefinitionInstantiated(const clang::FunctionDecl *D) override;
};

class DiagMuter
{
public:
   DiagMuter();
   ~DiagMuter();
};

class PCH
{
public:
    Strings headers; // array of all C/C++ header names with the "" or <>, required as long as we're using a PCH
            // the array is initialized at the first Modmap::semantic and kept in sync with a cache file named 'calypso_cache.list'
            // TODO: it's currently pretty basic and dumb and doesn't check whether the same header might be named differently or is already included by another
    bool needHeadersReload = false;
    ASTUnit *AST = nullptr;
    clang::MangleContext *MangleCtx = nullptr;

    DiagnosticPrinter *DiagClient;
    clang::IntrusiveRefCntPtr<clang::DiagnosticsEngine> Diags;
    
    ModuleMap *MMap = nullptr;

    void init(); // load the list of headers already cached in the PCH
    void add(const char* header, ::Module *from);

    void update(); // re-emit the PCH if needed, and update the cached list

    bool needSaving = false;
    void save();

    std::string pchHeader;
    std::string pchFilename;
//     std::string pchFilenameNew; // the PCH may be updated by Calypso, but into a different file since the original PCH is still opened as external source for the ASTContext

protected:
    void loadFromHeaders();
    void loadFromPCH();
};

class LangPlugin : public ::LangPlugin, public ::ForeignCodeGen
{
public:
    // ==== LangPlugin ====
    // returns -1 if said lang isn't handled by this plugin, or its id number
    // to be passed to createImport otherwise
    int doesHandleModmap(const utf8_t *lang) override;

    ::Modmap *createModmap(int langId,
        Loc loc, Expression *arg) override;

    // returns -1 if said tree isn't handled by this plugin, or its id number
    // to be passed to createImport otherwise
    int doesHandleImport(const utf8_t *tree) override;

    ::Import *createImport(int treeId,
        Loc loc, Identifiers *packages, Identifier *id,
        Identifier *aliasId, int isstatic) override;

    bool doesHandleCatch(LINK lang) override;
    ::Catch *createCatch(Loc loc, Type *t, Identifier *id,
                               Statement *handler, StorageClass stc) override;

    const char *mangle(Dsymbol *s) override;
    void mangleAnonymousAggregate(OutBuffer *buf, ::AggregateDeclaration* ad) override;
    Visitor *getForeignMangler(OutBuffer *buf, bool forEquiv, Visitor *base) override;

    Expression *getRightThis(Loc loc, Scope *sc, ::AggregateDeclaration *ad,
        Expression *e1, Declaration *var, int flag = 0) override;
    Expression *callCpCtor(Scope *sc, Expression *e) override;

    ::FuncDeclaration *buildDtor(::AggregateDeclaration *ad, Scope *sc) override;
    ::FuncDeclaration *buildOpAssign(StructDeclaration *sd, Scope *sc) override;

    // ==== CodeGen ====
    ForeignCodeGen *codegen() override { return this; }
    bool needsCodegen(::Module *m) override;

    std::stack<clangCG::CodeGenFunction *> CGFStack;
    inline clangCG::CodeGenFunction *CGF() { return CGFStack.top(); }

    void enterModule(::Module *m, llvm::Module *) override;
    void leaveModule(::Module *m, llvm::Module *) override;

    void enterFunc(::FuncDeclaration *fd) override;
    void leaveFunc() override;

    LLType *toType(Type *t) override;
    llvm::FunctionType *toFunctionType(::FuncDeclaration *fdecl) override;
    llvm::Type *IrTypeStructHijack(::StructDeclaration *sd) override;

    LLConstant *toConstExpInit(Loc loc, Type *targetType, Expression *exp) override;

    llvm::Constant *createInitializerConstant(IrAggr *irAggr,
        const IrAggr::VarInitMap& explicitInitializers,
        llvm::StructType* initializerType = 0) override;
    void addFieldInitializers(llvm::SmallVectorImpl<llvm::Constant*>& constants,
        const IrAggr::VarInitMap& explicitInitializers, ::AggregateDeclaration* decl,
        unsigned& offset, bool populateInterfacesWithVtbls) override;

    void toResolveFunction(::FuncDeclaration* fdecl) override;
    void toDefineFunction(::FuncDeclaration* fdecl) override;
    void toDeclareVariable(::VarDeclaration* vd) override;
    void toDefineVariable(::VarDeclaration* vd) override;
    void toDefineStruct(::StructDeclaration* sd) override;
    void toDefineClass(::ClassDeclaration* cd) override;

    bool toIsReturnInArg(CallExp* ce) override;
    LLValue *toVirtualFunctionPointer(DValue* inst, ::FuncDeclaration* fdecl, char* name) override;
    DValue* toCallFunction(Loc& loc, Type* resulttype, DValue* fnval,
                                   Expressions* arguments, llvm::Value *retvar) override;

    bool toConstructVar(::VarDeclaration *vd, llvm::Value *value, Expression *rhs) override;

    LLValue* toIndexAggregate(LLValue* src, ::AggregateDeclaration* ad, ::VarDeclaration* vd, Type *srcType) override;
    void addBaseClassData(AggrTypeBuilder &builder, ::AggregateDeclaration *base) override;
    void emitAdditionalClassSymbols(::ClassDeclaration *cd) override;
    void toInitClass(TypeClass* tc, LLValue* dst) override;

    void toPostNewClass(Loc& loc, TypeClass* tc, DValue* val) override;

    void toBeginCatch(IRState *irs, ::Catch *cj) override;
    void toEndCatch(IRState *irs, ::Catch *cj) override;
    llvm::Constant *toCatchScopeType(IRState *irs, Type *t) override;

    void EmitInternalDeclsForFields(const clang::RecordDecl *RD);
         
    // ==== ==== ====
    PCH pch;
    llvm::MapVector<const clang::Decl*, std::string> MangledDeclNames;

    typedef std::vector<std::pair<const clang::IdentifierInfo*, clang::Expr*>> MacroMapEntryTy;
    llvm::DenseMap<const clang::Module::Header*, MacroMapEntryTy*> MacroMap;

    BuiltinTypes &builtinTypes;
    DeclReferencer &declReferencer;

    ::ClassDeclaration *type_info_ptr; // wrapper around std::type_info for EH
    std::map<llvm::Constant*, llvm::GlobalVariable*> type_infoWrappers; // FIXME put into module state with the CodeGenModule

    std::string executablePath; // from argv[0] to locate Clang builtin headers

    struct GenModSet : public llvm::StringSet<> // already compiled modules
    {
        bool parsed = false;

        void parse();
        void add(::Module *m);
    } genModSet;

    // settings
    const char *cachePrefix = "calypso_cache"; // prefix of cached files (list of headers, PCH)

    llvm::SmallVector<const clang::VarDecl*, 4> EmittedStaticVars; // static variables emitted at CodeGenModule::Release that need their linkage fixed
    std::unique_ptr<clangCG::CodeGenModule> CGM;  // selectively emit external C++ declarations, template instances, ...

    LangPlugin();
    void init(const char *Argv0);

    void buildMacroMap();

    ASTUnit *getASTUnit() { return pch.AST; }
    clang::ASTContext &getASTContext();
    clang::Sema &getSema();
    clang::DiagnosticsEngine &getDiagnostics();
    clang::Preprocessor &getPreprocessor();
    clang::SourceManager &getSourceManager();

    std::string getCacheFilename(const char *suffix = nullptr);

    // FIXME quick&dirty traits addition
    Expression *semanticTraits(TraitsExp *e, Scope *sc);
    
private:
    void updateCGFInsertPoint();    // CGF has its own IRBuilder, it's not an issue if we set its insert point correctly

    // Keep the existing LLVM types generated by CodeGenTypes between modules
    llvm::DenseMap<const clang::Type*, clangCG::CGRecordLayout*> CGRecordLayouts;
    llvm::DenseMap<const clang::Type*, llvm::StructType*> RecordDeclTypes;
    llvm::DenseMap<const clang::Type *, llvm::Type*> TypeCache;
};

extern LangPlugin calypso;
bool isCPP(Type* t);
bool isCPP(Dsymbol *s);
cpp::ClassDeclaration *isDCXX(Dsymbol *s);

}

#define CALYPSO_LANGPLUGIN \
    ::LangPlugin *langPlugin() override { return &calypso; }

#endif /* DMD_CPP_CALYPSO_H */
