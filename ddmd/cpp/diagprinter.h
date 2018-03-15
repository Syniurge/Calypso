//===--- TextDiagnosticPrinter.h - Text Diagnostic Client -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is a concrete diagnostic client, which prints the diagnostics to
// standard error.
//
// CALYPSO NOTE: a flavoring was needed because we want to silent messages
// when attempting an instantiation, but we still want the error counter to increment.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/LLVM.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include <memory>

namespace clang {
class DiagnosticOptions;
class LangOptions;
class TextDiagnostic;
}

namespace cpp {
namespace reclang {
using namespace clang;

class DiagnosticPrinter : public DiagnosticConsumer {
  raw_ostream &OS;
  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts;

  /// \brief Handle to the currently active text diagnostic emitter.
  std::unique_ptr<TextDiagnostic> TextDiag;

  /// A string to prefix to error messages.
  std::string Prefix;

  unsigned OwnsOutputStream : 1;

public:
  DiagnosticPrinter(raw_ostream &os, DiagnosticOptions *diags,
                        bool OwnsOutputStream = false);
  ~DiagnosticPrinter() override;

  /// setPrefix - Set the diagnostic printer prefix string, which will be
  /// printed at the start of any diagnostics. If empty, no prefix string is
  /// used.
  void setPrefix(std::string Value) { Prefix = Value; }

  void BeginSourceFile(const LangOptions &LO, const Preprocessor *PP) override;
  void EndSourceFile() override;
  void HandleDiagnostic(DiagnosticsEngine::Level Level,
                        const Diagnostic &Info) override;

  // CALYPSO addition
  bool muted = true;
};

} // end namespace reclang
} // end namespace cpp
