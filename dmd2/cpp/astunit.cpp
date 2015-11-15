//===--- ASTUnit.cpp - ASTUnit utility ------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// ASTUnit Implementation.
//
//===----------------------------------------------------------------------===//

#include "astunit.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeOrdering.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/VirtualFileSystem.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Frontend/FrontendOptions.h"
#include "clang/Frontend/MultiplexConsumer.h"
#include "clang/Frontend/Utils.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Sema/Sema.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/CrashRecoveryContext.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/MutexGuard.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/raw_ostream.h"
#include <atomic>
#include <cstdio>
#include <cstdlib>

using namespace clang;

namespace {

/// \brief Gathers information from ASTReader that will be used to initialize
/// a Preprocessor.
class ASTInfoCollector : public ASTReaderListener {
  Preprocessor &PP;
  ASTContext &Context;
  LangOptions &LangOpt;
  std::shared_ptr<TargetOptions> &TargetOpts;
  IntrusiveRefCntPtr<TargetInfo> &Target;
  unsigned &Counter;

  bool InitializedLanguage;
public:
  ASTInfoCollector(Preprocessor &PP, ASTContext &Context, LangOptions &LangOpt,
                   std::shared_ptr<TargetOptions> &TargetOpts,
                   IntrusiveRefCntPtr<TargetInfo> &Target, unsigned &Counter)
      : PP(PP), Context(Context), LangOpt(LangOpt), TargetOpts(TargetOpts),
        Target(Target), Counter(Counter), InitializedLanguage(false) {}

  bool ReadLanguageOptions(const LangOptions &LangOpts, bool Complain,
                           bool AllowCompatibleDifferences) override {
    if (InitializedLanguage)
      return false;

    LangOpt = LangOpts;
    InitializedLanguage = true;

    updated();
    return false;
  }

  bool ReadTargetOptions(const TargetOptions &TargetOpts,
                         bool Complain) override {
    // If we've already initialized the target, don't do it again.
    if (Target)
      return false;

    this->TargetOpts = std::make_shared<TargetOptions>(TargetOpts);
    Target =
        TargetInfo::CreateTargetInfo(PP.getDiagnostics(), this->TargetOpts);

    updated();
    return false;
  }

  void ReadCounter(const serialization::ModuleFile &M,
                   unsigned Value) override {
    Counter = Value;
  }

private:
  void updated() {
    if (!Target || !InitializedLanguage)
      return;

    // Inform the target of the language options.
    //
    // FIXME: We shouldn't need to do this, the target should be immutable once
    // created. This complexity should be lifted elsewhere.
    Target->adjust(LangOpt);

    // Initialize the preprocessor.
    PP.Initialize(*Target);

    // Initialize the ASTContext
    Context.InitBuiltinTypes(*Target);

    // We didn't have access to the comment options when the ASTContext was
    // constructed, so register them now.
    Context.getCommentCommandTraits().registerCommentOptions(
        LangOpt.CommentOpts);
  }
};

}

namespace cpp
{

namespace reclang
{

ASTUnit::~ASTUnit() {
  // If we loaded from an AST file, balance out the BeginSourceFile call.
  if (getDiagnostics().getClient()) {
    getDiagnostics().getClient()->EndSourceFile();
  }
}

void ASTUnit::findFileRegionDecls(FileID File, unsigned Offset, unsigned Length,
                                  SmallVectorImpl<Decl *> &Decls) {
  if (File.isInvalid())
    return;

  if (SourceMgr->isLoadedFileID(File)) {
    assert(Ctx->getExternalSource() && "No external source!");
    return Ctx->getExternalSource()->FindFileRegionDecls(File, Offset, Length,
                                                         Decls);
  }

  llvm_unreachable("PCH decls should always be external");
}

/// \brief Configure the diagnostics object for use with ASTUnit.
void ASTUnit::ConfigureDiags(IntrusiveRefCntPtr<DiagnosticsEngine> &Diags,
                             const char **ArgBegin, const char **ArgEnd,
                             ASTUnit &AST) {
  if (!Diags.get()) {
    // No diagnostics engine was provided, so create our own diagnostics object
    // with the default options.
    DiagnosticConsumer *Client = nullptr;
    Diags = CompilerInstance::createDiagnostics(new DiagnosticOptions(),
                                                Client,
                                                /*ShouldOwnClient=*/true);
  }
}

ASTUnit *ASTUnit::LoadFromASTFile(const std::string &Filename,
                              IntrusiveRefCntPtr<DiagnosticsEngine> Diags,
                                  const FileSystemOptions &FileSystemOpts,
                                  ASTConsumer *Consumer,
                                  ASTReader::ASTReadResult &ReadResult,
                                  bool OnlyLocalDecls) {
  std::unique_ptr<ASTUnit> AST(new ASTUnit);

  // Recover resources if we crash before exiting this method.
  llvm::CrashRecoveryContextCleanupRegistrar<ASTUnit>
    ASTUnitCleanup(AST.get());
  llvm::CrashRecoveryContextCleanupRegistrar<DiagnosticsEngine,
    llvm::CrashRecoveryContextReleaseRefCleanup<DiagnosticsEngine> >
    DiagCleanup(Diags.get());

  ConfigureDiags(Diags, nullptr, nullptr, *AST);

  AST->OnlyLocalDecls = OnlyLocalDecls;
  AST->Diagnostics = Diags;
  IntrusiveRefCntPtr<vfs::FileSystem> VFS = vfs::getRealFileSystem();
  AST->FileMgr = new FileManager(FileSystemOpts, VFS);
  AST->SourceMgr = new SourceManager(AST->getDiagnostics(),
                                     AST->getFileManager(),
                                     false);
  AST->HSOpts = new HeaderSearchOptions();

  AST->HeaderInfo.reset(new HeaderSearch(AST->HSOpts,
                                         AST->getSourceManager(),
                                         AST->getDiagnostics(),
                                         AST->ASTFileLangOpts,
                                         /*Target=*/nullptr));

  PreprocessorOptions *PPOpts = new PreprocessorOptions();

  // Gather Info for preprocessor construction later on.

  HeaderSearch &HeaderInfo = *AST->HeaderInfo.get();
  unsigned Counter;

  AST->PP =
      new Preprocessor(PPOpts, AST->getDiagnostics(), AST->ASTFileLangOpts,
                       AST->getSourceManager(), HeaderInfo, *AST,
                       /*IILookup=*/nullptr,
                       /*OwnsHeaderSearch=*/false);
  Preprocessor &PP = *AST->PP;

  AST->Ctx = new ASTContext(AST->ASTFileLangOpts, AST->getSourceManager(),
                            PP.getIdentifierTable(), PP.getSelectorTable(),
                            PP.getBuiltinInfo());
  ASTContext &Context = *AST->Ctx;

  bool disableValid = false;
  if (::getenv("LIBCLANG_DISABLE_PCH_VALIDATION"))
    disableValid = true;
  AST->Reader = new ASTReader(PP, Context,
                             /*isysroot=*/"",
                             /*DisableValidation=*/disableValid,
                             false);

  AST->Reader->setListener(llvm::make_unique<ASTInfoCollector>(
      *AST->PP, Context, AST->ASTFileLangOpts, AST->TargetOpts, AST->Target,
      Counter));

  ReadResult = AST->Reader->ReadAST(Filename, serialization::MK_MainFile,
                          SourceLocation(), ASTReader::ARR_None);
  switch (ReadResult) {
  case ASTReader::Success:
    break;

  case ASTReader::Failure:
  case ASTReader::Missing:
  case ASTReader::OutOfDate:
  case ASTReader::VersionMismatch:
  case ASTReader::ConfigurationMismatch:
  case ASTReader::HadErrors:
    AST->getDiagnostics().Report(diag::err_fe_unable_to_load_pch);
    return nullptr;
  }

  PP.setCounterValue(Counter);

  // Attach the AST reader to the AST context as an external AST
  // source, so that declarations will be deserialized from the
  // AST file as needed.
  Context.setExternalSource(AST->Reader);

  AST->Consumer = Consumer;

  // Create a semantic analysis object and tell the AST reader about it.
  AST->TheSema.reset(new Sema(PP, Context, *AST->Consumer));
  AST->TheSema->Initialize();
  AST->Reader->InitializeSema(*AST->TheSema);

  // Tell the diagnostic client that we have started a source file.
  AST->getDiagnostics().getClient()->BeginSourceFile(Context.getLangOpts(),&PP);

  return AST.release();
}

}
}