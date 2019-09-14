// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CALYPSO_H
#define DMD_CPP_CALYPSO_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "mars.h"
#include "import.h"
#include "gen/cgforeign.h"

#include <memory>
#include <string>
#include "llvm/ADT/StringSet.h"
#include "llvm/IR/DataLayout.h"
#include "clang/AST/ASTMutationListener.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/CodeGen/ModuleBuilder.h"
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
class ModuleMap;
class PCHContainerOperations;
class DirectoryLookup;
namespace driver { class Compilation; class Driver; }
}

namespace cpp
{

class ClassDeclaration;
class BuiltinTypes;
class TemplateInstance;
class DeclMapper;

namespace reclang { class DiagnosticPrinter; }
using clang::ModuleMap;
using reclang::DiagnosticPrinter;

namespace clangCG = clang::CodeGen;

/***********************/

struct DData
{
    Dsymbol* sym = nullptr;
    bool hasSym = false; // TODO replace by llvm::PointerIntPair (once Callisto is stable)

    Dsymbol* wrapper = nullptr; // template declarations for partial/explicit template specializations or overloaded operators

    Dsymbol* instantiatedBy = nullptr; // if the declaration hasn't been mapped but was referenced/instantiated (i.e sym is null), it will get attached to the instantiating module or function
};

/***********************/

struct SpecValue
{
    const char *op = nullptr; // for overloaded operators
    Type *t = nullptr; // for conversion operators

    DeclMapper &mapper;
    SpecValue(DeclMapper &mapper) : mapper(mapper) {}

    operator bool() { return op || t; }
    RootObject *toTemplateArg(Loc loc);
};

Identifier *idPool(const char *s);

Identifier *prefixConflictingIdentifier(Identifier *ident);
Identifier *fromIdentifier(const clang::IdentifierInfo *II);
Identifier *fromDeclarationName(const clang::DeclarationName N, SpecValue *spec = nullptr);
Identifier *getIdentifier(const clang::NamedDecl* D, cpp::SpecValue* spec = nullptr, bool useCanonicalType = false);
Identifier *getIdentifierOrNull(const clang::NamedDecl* D, cpp::SpecValue* spec = nullptr, bool useCanonicalType = false);
Identifier *getExtendedIdentifier(const clang::NamedDecl *D, DeclMapper &mapper); // will return the name of the non-templated method for operators, same than getIdentifier() for other Decls
Identifier *getExtendedIdentifierOrNull(const clang::NamedDecl *D, DeclMapper &mapper);

RootObject *getIdentOrTempinst(Loc loc, const clang::DeclarationName N,
                               DeclMapper &mapper);

const clang::TagDecl *isOverloadedOperatorWithTagOperand(const clang::Decl *D,
                                                           const clang::NamedDecl *SpecificTag = nullptr);

Loc fromLoc(clang::SourceLocation L);

/***********************/

class PCH
{
public:
    Strings headers; // array of all C/C++ header names with the "" or <>, required as long as we're using a PCH
            // the array is initialized at the first pragma(cppmap, ...).importAll and kept in sync with a cache file named 'calypso_cache.list'
    size_t nextHeader;

    std::unique_ptr<clang::ASTUnit> AST;
    clang::MangleContext *MangleCtx = nullptr;

    DiagnosticPrinter *DiagClient;
    clang::IntrusiveRefCntPtr<clang::DiagnosticsEngine> Diags;
    std::shared_ptr<clang::PCHContainerOperations> PCHContainerOps;

    void init(); // load the list of headers already cached in the PCH
    void add(const char* header, ::Module *from);

    void update(); // re-emit the PCH if needed, and update the cached list

    bool needSaving = false;
    void save();

    std::string pchHeader;
    std::string pchFilename;

protected:
    void loadFromHeaders();
    void loadFromPCH();
    void loadNewHeaders();

    const clang::FileEntry* lookupHeader(const char* header, const clang::DirectoryLookup*& CurDir);
};

/***********************/

class LangPlugin final : public ::LangPlugin, public ::ForeignCodeGen
{
public:
    void _init() override;

    // ==== LangPlugin ====

    // returns -1 if said lang isn't handled by this plugin, or its id number
    // to be passed to createImport otherwise
    int doesHandleImport(const char* lang) override;

    ::Import *createImport(int langId,
        Loc loc, Identifiers *packages, Identifier *id,
        Identifier *aliasId, int isstatic) override;

    void dsymbolSemantic(Dsymbol *dsym, Scope *sc) override;

    bool isForeignInstance(::TemplateInstance *ti) override;

    int getPragma(Scope* sc, PragmaDeclaration* decl) override;
    void pragmaSemantic(Scope* sc, PragmaDeclaration* decl) override;

    bool doesHandleCatch(LINK lang) override;
    ::Catch *createCatch(Loc loc, Type *t, Identifier *id,
                               Statement *handler, StorageClass stc) override;

    Expression *callCpCtor(Scope *sc, Expression *e) override;

    ::DtorDeclaration *buildDtor(::AggregateDeclaration *ad, Scope *sc) override;
    ::FuncDeclaration *searchOpEqualsForXopEquals(::StructDeclaration *sd, Scope *sc) override;

    bool isSymbolReferenced(Dsymbol *s) override;
    void markSymbolReferenced(Dsymbol *s) override;

    Expression *semanticTraits(TraitsExp *e, Scope *sc) override;

    Expression *interpret(FuncDeclaration *fd, InterState *istate, Expressions *arguments,
                                  Expression *thisarg) override;
    bool canInterpret(FuncDeclaration *fd) override;

    void adjustLinkerArgs(std::vector<std::string>& args) override;

    // ==== mars_mainBody ====
    void codegenModules() override;

    // ==== CodeGen ====
    ForeignCodeGen *codegen() override { return this; }
    bool needsCodegen(::Module *m) override;

    std::stack<clangCG::CodeGenFunction *> CGFStack;
    inline clangCG::CodeGenFunction *CGF() { return CGFStack.top(); }

    void enterModule(::Module *m, llvm::Module *) override;
    void leaveModule(::Module *m, llvm::Module *) override;

    void enterFunc(::FuncDeclaration *fd) override;
    void leaveFunc() override;

    bool isEmitted(Dsymbol* s) override;

    LLType *toType(Type *t) override;
    llvm::FunctionType *toFunctionType(::FuncDeclaration *fdecl) override;
    bool passAggregateArgumentByRef(AggregateDeclaration* ad) override;
    llvm::Type *IrTypeStructHijack(::StructDeclaration *sd) override;

    llvm::Constant *createInitializerConstant(IrAggr *irAggr,
        const std::map<VarDeclaration *, llvm::Constant *>& explicitInitializers) override;
    llvm::Constant *createStructLiteralConstant(StructLiteralExp *e) override;
    llvm::Constant *toConstElemFallback(Expression *e) override;
    bool addFieldInitializers(llvm::SmallVectorImpl<llvm::Constant*>& constants,
        const std::map<VarDeclaration *, llvm::Constant *>& explicitInitializers, ::AggregateDeclaration* decl,
        unsigned& offset, bool populateInterfacesWithVtbls) override;

    void toResolveFunction(::FuncDeclaration* fdecl) override;
    void toDefineFunction(::FuncDeclaration* fdecl) override;
    void toDeclareVariable(::VarDeclaration* vd) override;
    void toDefineVariable(::VarDeclaration* vd) override;
    void toDefineStruct(::StructDeclaration* sd) override;
    void toDefineClass(::ClassDeclaration* cd) override;

    bool toIsReturnInArg(CallExp* ce) override;
    LLValue *toVirtualFunctionPointer(DValue* inst, ::FuncDeclaration* fdecl, const char* name) override;
    DValue* toCallFunction(Loc& loc, Type* resulttype, DValue* fnval,
                           Expressions *arguments, llvm::Value *retvar) override;

    bool toConstructVar(::VarDeclaration *vd, llvm::Value *value, Expression *rhs) override;

    LLValue* toIndexAggregate(LLValue* src, ::AggregateDeclaration* ad, ::VarDeclaration* vd) override;
    void addBaseClassData(AggrTypeBuilder &builder, ::AggregateDeclaration *base) override;
    void emitAdditionalClassSymbols(::ClassDeclaration *cd) override;
    void toInitClass(TypeClass* tc, LLValue* dst) override;
    DValue *toDynamicCast(Loc &loc, DValue *val, Type *_to) override;
    DValue *adjustForDynamicCast(Loc &loc, DValue *val, Type *_to) override;

    void toPostNewClass(Loc& loc, TypeClass* tc, DValue* val) override;

    void toBeginCatch(IRState& irs, ::Catch *cj) override;
    void toEndCatch(IRState& irs, ::Catch *cj) override;
    llvm::GlobalVariable* toCatchScopeType(IRState& irs, Type *t) override;

    // ==== ==== ====
    PCH pch;
    llvm::StringSet<> TargetFeatures;

    BuiltinTypes &builtinTypes;

    ::ClassDeclaration *type_info_ptr; // wrapper around std::type_info for EH
    std::map<llvm::Constant*, llvm::GlobalVariable*> type_infoWrappers; // FIXME put into module state with the CodeGenModule

    const char *Argv0;

    Identifier* id_cppmap;

    Identifier* id_cpp;
    Identifier* id_core;
    Identifier* id__;
    Identifier* id_Scpp;

    Identifier* id_isCpp;
    Identifier* id_getCppVirtualIndex;
    Identifier* id_getBaseOffset;
    Identifier* id_getMemberPointerExtraSlots;

    Identifier *id___cpp_type_info_ptr;
    Identifier* id_cpp_member_ptr;

    Identifier* id_op;
    Identifier* id_type;

    Identifier* id___va_list_tag;
    Identifier* id___NSConstantString_tag;

    int cxxStdlibType;

    // settings
    const char *cachePrefix = "calypso_cache"; // prefix of cached files (list of headers, PCH)

    llvm::SmallVector<const char*, 16> clangArgv;
    std::unique_ptr<clang::driver::Driver> TheDriver;
    clang::driver::Compilation* buildClangCompilation();

    std::unique_ptr<clangCG::CodeGenModule> CGM;  // selectively emit external C++ declarations, template instances, ...

    LangPlugin();

    clang::ASTUnit *getASTUnit() { return pch.AST.get(); }
    clang::ASTContext &getASTContext();
    clang::Sema &getSema();
    clang::DiagnosticsEngine &getDiagnostics();
    clang::Preprocessor &getPreprocessor();
    clang::SourceManager &getSourceManager();
    clang::FileManager &getFileManager();

    std::string getCacheFilename(const char *suffix = nullptr);

    void mangle(const clang::NamedDecl *D, std::string& str);
    void mangleAnonymousAggregate(::AggregateDeclaration* ad, OutBuffer *buf);

    std::unordered_map<const Identifier*, clang::IdentifierInfo*> IIMap;

    clang::IdentifierInfo* toIdentifierInfo(Identifier* ident); // remove the 'ℂ' if present
    clang::DeclarationName toDeclarationName(Identifier* ident, const clang::RecordDecl* RD = nullptr);

private:
    void updateCGFInsertPoint();    // CGF has its own IRBuilder, it's not an issue if we set its insert point correctly

    // Keep the existing LLVM types generated by CodeGenTypes between modules
    llvm::DenseMap<const clang::Type*, clangCG::CGRecordLayout*> CGRecordLayouts;
    llvm::DenseMap<const clang::Type*, llvm::StructType*> RecordDeclTypes;
    llvm::DenseMap<const clang::Type *, llvm::Type*> TypeCache;
};

/***********************/

extern LangPlugin calypso;
bool isCPP(Type* t);
bool isCPP(Dsymbol *s);
cpp::ClassDeclaration *isDCXX(Dsymbol *s);

}

#define CALYPSO_LANGPLUGIN \
    ::LangPlugin *langPlugin() override { return &calypso; }

// Use this pattern for all global.params.verbose logging
void log_verbose(const std::string& header, const std::string& msg);

void cppAddMember(Dsymbol* dsym, Scope* sc, ScopeDsymbol* sds);
void cppSemantic(Dsymbol* dsym, Scope* sc);

#endif /* DMD_CPP_CALYPSO_H */
