//===--- ASTUnit.h - ASTUnit utility ----------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// ASTUnit utility class.
//
// CALYPSO NOTE: Flavouring ASTUnit is necessary because Clang doesn't provide any way to assign a custom ASTConsumer.
// Fortunately the functionality we use is only a few members + LoadFromASTFile().
//
//===----------------------------------------------------------------------===//

#pragma once

#include "clang/AST/ASTContext.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/FileSystemOptions.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Lex/ModuleLoader.h"
#include "clang/Lex/PreprocessingRecord.h"
#include "clang/Sema/CodeCompleteConsumer.h"
#include "clang/Serialization/ASTBitCodes.h"
#include "clang/Serialization/ASTReader.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/MD5.h"
#include "llvm/Support/Path.h"
#include <cassert>
#include <map>
#include <memory>
#include <string>
#include <sys/types.h>
#include <utility>
#include <vector>

namespace clang
{
class Sema;
class ASTContext;
class CodeCompleteConsumer;
class CompilerInvocation;
class CompilerInstance;
class Decl;
class DiagnosticsEngine;
class FileEntry;
class FileManager;
class HeaderSearch;
class Preprocessor;
class SourceManager;
class TargetInfo;
class ASTFrontendAction;
class ASTDeserializationListener;
}

namespace cpp
{

namespace reclang
{

using namespace clang;

/// \brief Utility class for loading a ASTContext from an AST file.
///
class ASTUnit : public ModuleLoader {
private:
  std::shared_ptr<LangOptions>            LangOpts;
  IntrusiveRefCntPtr<DiagnosticsEngine>   Diagnostics;
  IntrusiveRefCntPtr<FileManager>         FileMgr;
  IntrusiveRefCntPtr<SourceManager>       SourceMgr;
  std::unique_ptr<HeaderSearch>           HeaderInfo;
  IntrusiveRefCntPtr<TargetInfo>          Target;
  IntrusiveRefCntPtr<Preprocessor>        PP;
  IntrusiveRefCntPtr<ASTContext>          Ctx;
  std::shared_ptr<TargetOptions>          TargetOpts;
  IntrusiveRefCntPtr<HeaderSearchOptions> HSOpts;
  IntrusiveRefCntPtr<ASTReader> Reader;

  FileSystemOptions FileSystemOpts;

  /// \brief The AST consumer that received information about the translation
  /// unit as it was parsed or loaded.
  ASTConsumer *Consumer;

  /// \brief The semantic analysis object used to type-check the translation
  /// unit.
  std::unique_ptr<Sema> TheSema;

  /// Optional owned invocation, just used to make the invocation used in
  /// LoadFromCommandLine available.
  IntrusiveRefCntPtr<CompilerInvocation> Invocation;

  // OnlyLocalDecls - when true, walking this AST should only visit declarations
  // that come from the AST itself, not from included precompiled headers.
  // FIXME: This is temporary; eventually, CIndex will always do this.
  bool                              OnlyLocalDecls;

  /// \brief What kind of translation unit this AST represents.
  TranslationUnitKind TUKind;

  /// \brief The language options used when we load an AST file.
  LangOptions ASTFileLangOpts;

public:
  ~ASTUnit();

  const DiagnosticsEngine &getDiagnostics() const { return *Diagnostics; }
  DiagnosticsEngine &getDiagnostics()             { return *Diagnostics; }

  const SourceManager &getSourceManager() const { return *SourceMgr; }
        SourceManager &getSourceManager()       { return *SourceMgr; }

  const Preprocessor &getPreprocessor() const { return *PP; }
        Preprocessor &getPreprocessor()       { return *PP; }

  const ASTContext &getASTContext() const { return *Ctx; }
        ASTContext &getASTContext()       { return *Ctx; }

  const TargetInfo &getTargetInfo() const { return *Target; }
        TargetInfo &getTargetInfo()       { return *Target; }

  const HeaderSearch &getHeaderSearch() const { return *HeaderInfo; }
        HeaderSearch &getHeaderSearch()       { return *HeaderInfo; }

  void setASTContext(ASTContext *ctx) { Ctx = ctx; }
  void setPreprocessor(Preprocessor *pp);

  bool hasSema() const { return (bool)TheSema; }
  Sema &getSema() const {
    assert(TheSema && "ASTUnit does not have a Sema object!");
    return *TheSema;
  }

  const LangOptions &getLangOpts() const {
    assert(LangOpts && " ASTUnit does not have language options");
    return *LangOpts;
  }

  const LangOptions &getASTFileLangOpts() const {
    return ASTFileLangOpts;
  }

  const FileManager &getFileManager() const { return *FileMgr; }
        FileManager &getFileManager()       { return *FileMgr; }

  /// \brief Get the decls that are contained in a file in the Offset/Length
  /// range. \p Length can be 0 to indicate a point at \p Offset instead of
  /// a range.
  void findFileRegionDecls(FileID File, unsigned Offset, unsigned Length,
                           SmallVectorImpl<Decl *> &Decls);

  static void ConfigureDiags(IntrusiveRefCntPtr<DiagnosticsEngine> &Diags,
                             const char **ArgBegin, const char **ArgEnd,
                             ASTUnit &AST);

  /// \brief Create a ASTUnit from an AST file.
  ///
  /// \param Filename - The AST file to load.
  ///
  /// \param Diags - The diagnostics engine to use for reporting errors; its
  /// lifetime is expected to extend past that of the returned ASTUnit.
  ///
  /// \returns - The initialized ASTUnit or null if the AST failed to load.
  static ASTUnit *LoadFromASTFile(const std::string &Filename,
                              IntrusiveRefCntPtr<DiagnosticsEngine> Diags,
                                  const FileSystemOptions &FileSystemOpts,
                                  ASTConsumer *Consumer,
                                  ASTReader::ASTReadResult &ReadResult,
                                  bool OnlyLocalDecls = false);

  ModuleLoadResult loadModule(SourceLocation ImportLoc, ModuleIdPath Path,
                              Module::NameVisibilityKind Visibility,
                              bool IsInclusionDirective) override {
    // ASTUnit doesn't know how to load modules (not that this matters).
    return ModuleLoadResult();
  }

  void makeModuleVisible(Module *Mod, Module::NameVisibilityKind Visibility,
                         SourceLocation ImportLoc, bool Complain) override {}

  GlobalModuleIndex *loadGlobalModuleIndex(SourceLocation TriggerLoc) override
    { return nullptr; }
  bool lookupMissingImports(StringRef Name, SourceLocation TriggerLoc) override
    { return 0; }
};

}
}
