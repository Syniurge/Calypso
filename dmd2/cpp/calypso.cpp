// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "cpp/modulemap.h"
#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/cppaggregate.h"
#include "cpp/cpptemplate.h"
#include "cpp/cpptypes.h"

#include "aggregate.h"
#include "declaration.h"
#include "expression.h"
#include "id.h"
#include "lexer.h"
#include "expression.h"

#include "driver/tool.h"
#include "driver/cl_options.h"

#include "clang/AST/DeclTemplate.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Tool.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/ModuleMap.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Serialization/ASTWriter.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/Program.h"
#include "llvm/IR/LLVMContext.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

LangPlugin calypso;
BuiltinTypes builtinTypes;

static inline ASTUnit* ast() { return calypso.pch.AST; }

RootObject *SpecValue::toTemplateArg(Loc loc)
{
    assert(op || t);
    if (op)
        return new StringExp(loc, const_cast<char*>(op));
    else
        return t;
}

Identifier *fromIdentifier(const clang::IdentifierInfo *II)
{
    return Lexer::idPool(II->getNameStart());
        // NOTE: Every C++ identifier passing through DMD gets its own redundant copy in memory
        // Is this the cost of interfacing with Clang or is there another way? (probably not an easy one)
}

static const char *getOperatorName(const clang::OverloadedOperatorKind OO)
{
    switch (OO)
    {
#   define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
        case clang::OO_##Name: return #Name;
#   include "clang/Basic/OperatorKinds.def"
        default: return "None";
    }
}

static const char *getDOperatorSpelling(const clang::OverloadedOperatorKind OO)
{
    switch (OO)
    {
        case clang::OO_PlusEqual: return "+";
        case clang::OO_MinusEqual: return "-";
        case clang::OO_StarEqual: return "*";
        case clang::OO_SlashEqual: return "/";
        case clang::OO_PercentEqual: return "%";
        case clang::OO_CaretEqual: return "^";
        case clang::OO_AmpEqual: return "&";
        case clang::OO_PipeEqual: return "|";
        case clang::OO_LessLessEqual: return "<<";
        case clang::OO_GreaterGreaterEqual: return ">>";
        default:
            return clang::getOperatorSpelling(OO);
    }
}

static Identifier *fullOperatorMapIdent(Identifier *baseIdent,
                                       const clang::FunctionDecl *FD)
{
    auto OO = FD->getOverloadedOperator();

    std::string fullName(baseIdent->string, baseIdent->len);
    fullName += "_";
    fullName += getOperatorName(OO);

    return Lexer::idPool(fullName.c_str());
}

static Identifier *getOperatorIdentifier(const clang::FunctionDecl *FD,
                const char *&op, clang::OverloadedOperatorKind OO = clang::OO_None)
{
    if (FD)
        OO = FD->getOverloadedOperator();

    Identifier *opIdent;
    bool wrapInTemp = false;

    auto MD = llvm::dyn_cast_or_null<clang::CXXMethodDecl>(FD);
    bool isNonMember = !MD || MD->isStatic();

    auto NumParams = FD->getNumParams();
    if (!isNonMember)
        NumParams++;

    if (OO == clang::OO_Call)
        opIdent = Id::call;
    else if(OO == clang::OO_Subscript)
        opIdent = Id::index;
    else
    {
        bool isUnary = NumParams == 1;
        bool isBinary = NumParams == 2;

        wrapInTemp = true; // except for opAssign and opCmp

        if (isUnary)
        {
            switch (OO)
            {
                case clang::OO_Plus:
                case clang::OO_Minus:
                case clang::OO_Star:
                case clang::OO_Tilde:
                case clang::OO_PlusPlus:
                case clang::OO_MinusMinus:
                    // operators without D equivalent need to be mapped for linking
                case clang::OO_Exclaim:
                case clang::OO_Arrow:
                case clang::OO_ArrowStar:
                    opIdent = Id::opUnary;
                    break;
                default:
                    ::warning(Loc(), "Ignoring C++ unary operator %s", clang::getOperatorSpelling(OO));
                    return nullptr;
            }
        }
        else if (isBinary)
        {
            switch (OO)
            {
                case clang::OO_Plus:
                case clang::OO_Minus:
                case clang::OO_Star:
                case clang::OO_Slash:
                case clang::OO_Percent:
                case clang::OO_Caret:
                case clang::OO_Amp:
                case clang::OO_Pipe:
                case clang::OO_Tilde:
                case clang::OO_LessLess:
                case clang::OO_GreaterGreater:
                    // operators without D equivalent need to be mapped for linking
                case clang::OO_PlusPlus:
                case clang::OO_MinusMinus:
                case clang::OO_Comma:
                    opIdent = Id::opBinary;
                    break;
                case clang::OO_EqualEqual:
                    opIdent = Id::eq;
                    wrapInTemp = false;
                    break;
                case clang::OO_ExclaimEqual:
                    opIdent = Lexer::idPool("opEqualsNot"); // TODO?
                    wrapInTemp = false;
                    break;
                case clang::OO_Less:
                case clang::OO_LessEqual:
                case clang::OO_Greater:
                case clang::OO_GreaterEqual:
                    opIdent = fullOperatorMapIdent(Id::cmp, FD);
                    wrapInTemp = false;
                    break;
                case clang::OO_Equal:
                    // NOTE: C++ assignment operators can't be non-members.
                    opIdent = Id::assign;
                    wrapInTemp = false;
                    break;
                case clang::OO_PlusEqual:
                case clang::OO_MinusEqual:
                case clang::OO_StarEqual:
                case clang::OO_SlashEqual:
                case clang::OO_PercentEqual:
                case clang::OO_CaretEqual:
                case clang::OO_AmpEqual:
                case clang::OO_PipeEqual:
                case clang::OO_LessLessEqual:
                case clang::OO_GreaterGreaterEqual:
                    opIdent = Id::opOpAssign;
                    break;
                default:
                    ::warning(Loc(), "Ignoring C++ binary operator %s", clang::getOperatorSpelling(OO));
                    return nullptr;
            }
        }
        else
            return nullptr; // operator new or delete (TODO linking)
    }

    op = wrapInTemp ? getDOperatorSpelling(OO) : nullptr;
    return opIdent;
}

static Identifier *fullConversionMapIdent(Identifier *baseIdent,
                                       const clang::CXXConversionDecl *D)
{
    auto& Context = calypso.getASTContext();
    auto MangleCtx = calypso.pch.MangleCtx;

    TypeMapper mapper;
    mapper.addImplicitDecls = false;

    auto T = D->getConversionType().getDesugaredType(Context);
    auto t = mapper.fromType(T, Loc());

    std::string fullName(baseIdent->string, baseIdent->len);
    fullName += "_";
    if (t->isTypeBasic()) // not too complex, use a readable suffix
    {
        auto TypeQuals = T.getCVRQualifiers();
        if (TypeQuals & clang::Qualifiers::Const) fullName += "const_";
        if (TypeQuals & clang::Qualifiers::Volatile) fullName += "volatile_";
        if (TypeQuals & clang::Qualifiers::Restrict) fullName += "restrict_";
        fullName += t->kind();
    }
    else // use the mangled name, rare occurrence and not a big deal if unreadable (only ever matters for virtual conversion operators)
    {
        llvm::raw_string_ostream OS(fullName);
        MangleCtx->mangleTypeName(T, OS);
        OS.flush();
    }

    return Lexer::idPool(fullName.c_str());
}

static Identifier *getConversionIdentifier(const clang::CXXConversionDecl *D,
                TypeMapper &mapper, Type *&t, clang::QualType T = clang::QualType())
{
    if (D)
        T = D->getConversionType();

    t = mapper.fromType(T, Loc());
    return Id::cast;
}

Identifier *fromDeclarationName(const clang::DeclarationName N,
                                    SpecValue *spec)
{
    switch (N.getNameKind())
    {
        case clang::DeclarationName::Identifier:
            return fromIdentifier(N.getAsIdentifierInfo());
        case clang::DeclarationName::CXXConstructorName:
            return Id::ctor;
        case clang::DeclarationName::CXXDestructorName:
            return Id::dtor; // NOTE: Id::dtor is the user-provided destructor code, "aggrDtor" the "true" destructor
        case clang::DeclarationName::CXXOperatorName:
        {
            assert(spec && "Operator name and spec isn't set");
            return getOperatorIdentifier(nullptr, spec->op,
                    N.getCXXOverloadedOperator());
        }
        case clang::DeclarationName::CXXConversionFunctionName:
        {
            assert(spec && "Conversion name and spec isn't set");
            return getConversionIdentifier(nullptr, spec->mapper,
                    spec->t, N.getCXXNameType());
        }
        default:
//             break;
            return nullptr;
    }

    llvm_unreachable("Unhandled DeclarationName");
}

Identifier *getIdentifierOrNull(const clang::NamedDecl *D, SpecValue *spec)
{
    if (auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D))
        D = FTD->getTemplatedDecl(); // same ident, can dyn_cast

    if (isa<clang::CXXConstructorDecl>(D))
        return Id::ctor;
    else if (isa<clang::CXXDestructorDecl>(D))
        return Id::dtor;
    else if (auto Conv = dyn_cast<clang::CXXConversionDecl>(D))
    {
        assert(spec);
        return getConversionIdentifier(Conv, spec->mapper, spec->t);
    }
    else if (auto FD = dyn_cast<clang::FunctionDecl>(D))
        if (FD->isOverloadedOperator())
        {
            assert(spec);
            return getOperatorIdentifier(FD, spec->op);
        }

    clang::IdentifierInfo *II = nullptr;

    if (D->getIdentifier())
        II = D->getIdentifier();
    else if (auto Tag = llvm::dyn_cast<clang::TagDecl>(D))
        if (auto Typedef = Tag->getTypedefNameForAnonDecl())
            II = Typedef->getIdentifier();

    if (!II)
        return nullptr;

    auto ident = fromIdentifier(II);

    if (isa<clang::RecordDecl>(D))
    {
        // Prefix reserved class names with '§'
        if (ident == Id::Object || ident == Id::Throwable || ident == Id::Exception || ident == Id::Error ||
            ident == Id::TypeInfo || ident == Id::TypeInfo_Class || ident == Id::TypeInfo_Interface ||
            ident == Id::TypeInfo_Struct || ident == Id::TypeInfo_Typedef || ident == Id::TypeInfo_Pointer ||
            ident == Id::TypeInfo_Array || ident == Id::TypeInfo_StaticArray || ident == Id::TypeInfo_AssociativeArray ||
            ident == Id::TypeInfo_Enum || ident == Id::TypeInfo_Function || ident == Id::TypeInfo_Delegate ||
            ident == Id::TypeInfo_Tuple || ident == Id::TypeInfo_Const || ident == Id::TypeInfo_Invariant ||
            ident == Id::TypeInfo_Shared || ident == Id::TypeInfo_Wild || ident == Id::TypeInfo_Vector) // thanks C++...
        {
            llvm::SmallString<48> s(u8"§"); // non-ASCII but pretty and available on most keyboards
            s += llvm::StringRef(ident->string, ident->len);
            ident = Lexer::idPool(s.c_str());
        }
    }

    return ident;
}

Identifier *getIdentifier(const clang::NamedDecl *D, SpecValue *spec)
{
    auto result = getIdentifierOrNull(D, spec);
    assert(result);

    return result;
}

Identifier *getExtendedIdentifierOrNull(const clang::NamedDecl *D,
                                  TypeMapper &mapper)
{
    SpecValue spec(mapper);
    auto ident = getIdentifier(D, &spec);
    if (!ident)
        return nullptr;

    auto FD = dyn_cast<clang::FunctionDecl>(D);
    if (spec.op && FD)
        ident = fullOperatorMapIdent(ident, FD);
    else if (spec.t)
        ident = fullConversionMapIdent(ident,
                    cast<clang::CXXConversionDecl>(D));

    return ident;
}

Identifier *getExtendedIdentifier(const clang::NamedDecl *D,
                                  TypeMapper &mapper)
{
    auto result = getExtendedIdentifierOrNull(D, mapper);
    assert(result);

    return result;
}

RootObject *getIdentOrTempinst(Loc loc, const clang::DeclarationName N,
                               TypeMapper &mapper)
{
    SpecValue spec(mapper);
    auto ident = fromDeclarationName(N, &spec);
    if (!ident)
        return nullptr;

    if (spec)
    {
        auto tempinst = new cpp::TemplateInstance(loc, ident);
        tempinst->tiargs = new Objects;
        tempinst->tiargs->push(spec.toTemplateArg(loc));
        return tempinst;
    }
    else
        return ident;
}

Loc fromLoc(clang::SourceLocation L)
{    Loc loc;

    clang::StringRef S(ast()->getSourceManager().getFilename(L));
    loc.filename.data = S.data();
    loc.filename.size = S.size();
    loc.linnum = ast()->getSourceManager().getSpellingLineNumber(L);

    return loc;
}

const clang::Decl *getDecl(Dsymbol *s)
{
    assert(isCPP(s));

#define RETRIEVE(DECL, MEMBER) \
    if (s->is##DECL()) return static_cast<cpp::DECL*>(s)->MEMBER;

    RETRIEVE(StructDeclaration, RD)
    RETRIEVE(ClassDeclaration, RD)
    RETRIEVE(EnumDeclaration, ED)
    RETRIEVE(CtorDeclaration, CCD)
    RETRIEVE(DtorDeclaration, CDD)
    RETRIEVE(FuncDeclaration, FD)
    RETRIEVE(VarDeclaration, VD)

#undef RETRIEVE
    llvm_unreachable("Unhandled getDecl");
}

/***********************/

// see CodeGenModule::getMangledName()
const char *LangPlugin::mangle(Dsymbol *s)
{
    assert(isCPP(s));

    if (s->isModule())
        return ::mangleImpl(s);

    auto ND = cast<clang::NamedDecl>(getDecl(s));

    auto &FoundStr = MangledDeclNames[ND->getCanonicalDecl()];
    if (!FoundStr.empty())
        return FoundStr.c_str();

    auto& Context = calypso.getASTContext();
    auto MangleCtx = pch.MangleCtx;

    llvm::SmallString<256> Buffer;
    llvm::StringRef Str;
    if (auto Tag = dyn_cast<clang::TagDecl>(ND)) {
        auto TagTy = Context.getTagDeclType(Tag);

        llvm::raw_svector_ostream Out(Buffer);
        MangleCtx->mangleTypeName(TagTy, Out);
//         Out << "_D"; // WARNING: mangleTypeName returns the RTTI typeinfo mangling
        Str = Out.str();
    } else if (MangleCtx->shouldMangleDeclName(ND)) {
        llvm::raw_svector_ostream Out(Buffer);
        if (const auto *D = dyn_cast<clang::CXXConstructorDecl>(ND))
            MangleCtx->mangleCXXCtor(D, clang::Ctor_Complete, Out);
        else if (const auto *D = dyn_cast<clang::CXXDestructorDecl>(ND))
            MangleCtx->mangleCXXDtor(D, clang::Dtor_Complete, Out);
        else
            MangleCtx->mangleName(ND, Out);
        Str = Out.str();
    } else {
        auto II = ND->getIdentifier();
        assert(II && "Attempt to mangle unnamed decl.");
        Str = II->getName();
    }

    Str.str().swap(FoundStr);
    return FoundStr.c_str();
}

/***********************/

void InstantiationChecker::CompletedImplicitDefinition(const clang::FunctionDecl *D)
{
    auto& Diags = calypso.pch.AST->getDiagnostics();

    calypso.pch.needSaving = true;

    if (Diags.hasErrorOccurred())
    {
//         if (!D->isInvalidDecl())
//             fprintf(stderr, "Marking %s invalid", D->getNameAsString().c_str());
//         const_cast<clang::FunctionDecl*>(D)->setInvalidDecl();
        Diags.Reset();
    }
}

void InstantiationChecker::FunctionDefinitionInstantiated(const clang::FunctionDecl *D)
{
    auto& Diags = calypso.pch.AST->getDiagnostics();

    calypso.pch.needSaving = true;

    if (Diags.hasErrorOccurred())
    {
//         if (!D->isInvalidDecl())
//             fprintf(stderr, "Marking %s invalid", D->getNameAsString().c_str());
//         const_cast<clang::FunctionDecl*>(D)->setInvalidDecl();
        Diags.Reset();
    }
}

/***********************/

#define MAX_FILENAME_SIZE 4096

void PCH::init()
{
    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> DiagOpts(new clang::DiagnosticOptions);
    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> DiagID(new clang::DiagnosticIDs);
    auto DiagClient = new clang::TextDiagnosticPrinter(llvm::errs(), &*DiagOpts);
    Diags = new clang::DiagnosticsEngine(DiagID,
                                         &*DiagOpts, DiagClient);

    auto& headerList = calypso.cachePrefix;

    auto fheaderList = fopen(headerList, "r"); // ordered list of headers
        // currently cached as one big PCH (neither modules nor chained PCH can be
        // used without modifying Clang).
    if (!fheaderList)
        return;

    char linebuf[MAX_FILENAME_SIZE];
    while (fgets(linebuf, MAX_FILENAME_SIZE, fheaderList) != NULL)
    {
        linebuf[strcspn(linebuf, "\n")] = '\0';
        if (linebuf[0] == '\0')
            continue;

        headers.push(strdup(linebuf));
    }

    fclose(fheaderList);
}

void PCH::add(StringRef header)
{
    bool found = false;

    for (unsigned i = 0; i < headers.dim; i++)
    {
        if (strncmp(header.data, headers[i], header.size) == 0)
        {
            found = true;
            break;
        }
    }

    if (found)
        return;

    headers.push(header);
    needEmit = true;
}

// WORKAROUND Temporary visitor to deserialize the entire ASTContext
class ASTDummyVisitor : public clang::RecursiveASTVisitor<ASTDummyVisitor>
{
public:
    bool shouldVisitTemplateInstantiations() const { return true; }
};

void PCH::update()
{
    auto& cachePrefix = calypso.cachePrefix;

    if (headers.empty())
        return;

    if (!needEmit && AST)
        return;

    // FIXME
    assert(!(needEmit && AST) && "Need AST merging FIXME");

    auto AddSuffixThenCheck = [&] (const char *suffix, bool dirtyPCH = true) {
        std::string fn_var(calypso.cachePrefix);
        fn_var += suffix;

        using namespace llvm::sys::fs;
        file_status result;
        status(fn_var, result);
        if (is_directory(result)) {
            ::error(Loc(), "%s is a directory\n", fn_var.c_str());
            fatal();
        }

        if (dirtyPCH && !exists(result))
            needEmit = true;

        return fn_var;
    };
    // NOTE: there's File::exists but it is incomplete and unused, hence llvm::sys::fs

    pchHeader = AddSuffixThenCheck(".h");
    pchFilename = AddSuffixThenCheck(".h.pch");
    pchFilenameNew = AddSuffixThenCheck(".new.pch", false);

    if (needEmit)
    {
        llvm::sys::fs::remove(pchFilenameNew, true);

        /* PCH generation */

        // Re-emit the source file with #include directives
        auto fmono = fopen(pchHeader.c_str(), "w");
        if (!fmono)
        {
            ::error(Loc(), "C++ monolithic header couldn't be created");
            fatal();
        }

        for (unsigned i = 0; i < headers.dim; ++i)
        {
            if (headers[i][0] == '<')
                fprintf(fmono, "#include %s\n", headers[i]);
            else
                fprintf(fmono, "#include \"%s\"\n", headers[i]);
        }

        fclose(fmono);

        // Compiler flags, we use a hack from clang-interpreter to extract -cc1 flags from "puny human" flags
        // The driver doesn't do anything except computing the flags.
        std::string TripleStr = llvm::sys::getProcessTriple();
        llvm::Triple T(TripleStr);

        clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> CC1DiagOpts(new clang::DiagnosticOptions);
        clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> CC1DiagID(new clang::DiagnosticIDs);
        auto CC1DiagClient = new clang::TextDiagnosticPrinter(llvm::errs(), &*CC1DiagOpts);
        clang::IntrusiveRefCntPtr<clang::DiagnosticsEngine> CC1Diags = new clang::DiagnosticsEngine(CC1DiagID,
                                            &*CC1DiagOpts, CC1DiagClient);

        clang::driver::Driver TheDriver(calypso.executablePath, T.str(), *CC1Diags);
        TheDriver.setTitle("Calypso PCH");

        llvm::SmallVector<const char *, 16> Argv;
        Argv.push_back("clang");
        for (auto cppArg: opts::cppArgs)
            Argv.push_back(cppArg.c_str());
        Argv.push_back("-x");
        Argv.push_back("c++-header");
        Argv.push_back("-Xclang");
        Argv.push_back("-emit-pch");
        Argv.push_back("-o");
        Argv.push_back(pchFilename.c_str());
        Argv.push_back(pchHeader.c_str());

        std::unique_ptr<clang::driver::Compilation> C(TheDriver.BuildCompilation(Argv));
        assert(C);

        // We expect to get back exactly one command job, if we didn't something
        // failed. Extract that job from the compilation.
        const clang::driver::JobList &Jobs = C->getJobs();
        assert(Jobs.size() == 1 && isa<clang::driver::Command>(*Jobs.begin()));
        const clang::driver::Command &Cmd = cast<clang::driver::Command>(*Jobs.begin());
        assert(llvm::StringRef(Cmd.getCreator().getName()) == "clang");

        // Initialize a compiler invocation object from the clang (-cc1) arguments.
        const clang::driver::ArgStringList &CCArgs = Cmd.getArguments();
        clang::CompilerInstance Clang;
        clang::CompilerInvocation::CreateFromArgs(Clang.getInvocation(), CCArgs.begin(), CCArgs.end(), *CC1Diags);
        Clang.createDiagnostics();
        assert(Clang.hasDiagnostics());

        // Infer the builtin include path if unspecified.
        if (Clang.getHeaderSearchOpts().UseBuiltinIncludes &&
                Clang.getHeaderSearchOpts().ResourceDir.empty())
            Clang.getHeaderSearchOpts().ResourceDir = TheDriver.ResourceDir;

        // Create and execute the frontend to generate a PCH
        std::unique_ptr<clang::GeneratePCHAction> Act(new clang::GeneratePCHAction);
        if (!Clang.ExecuteAction(*Act))
        {
            ::error(Loc(), "PCH generation failed!");
            fatal();
        }

        /* Update the list of headers */

        auto fheaderlist = fopen(cachePrefix, "w");
        if (fheaderlist == NULL)
        {
            ::error(Loc(), "C/C++ header list cache file couldn't be opened/created");
            fatal();
        }

        for (unsigned i = 0; i < headers.dim; ++i)
            fprintf(fheaderlist, "%s\n", headers[i]);

        fclose(fheaderlist);

        /* Mark every C++ module object file dirty */

        std::string genListFilename(cachePrefix);
        genListFilename += ".gen";
        llvm::sys::fs::remove(genListFilename, true);
    }

    needEmit = false;

    /* PCH was generated successfully, let's load it */

    // If the PCH was updated by Calypso to avoid redoing implicit instantiations, replace the old PCH
    // It cannot be overridden if loaded by an ASTContext, hence we're only doing it now.
    if (llvm::sys::fs::exists(pchFilenameNew))
    {
        llvm::sys::fs::remove(pchFilename, true);
        llvm::sys::fs::rename(pchFilenameNew, pchFilename);
    }

    clang::FileSystemOptions FileSystemOpts;

    AST = ASTUnit::LoadFromASTFile(pchFilename,
                                Diags, FileSystemOpts, &instChecker);

    // WORKAROUND for https://llvm.org/bugs/show_bug.cgi?id=24420
    // « RecordDecl::LoadFieldsFromExternalStorage() expels existing decls from the DeclContext linked list »
    // This only concerns serialized declarations, new records aren't affected by this issue
    ASTDummyVisitor().TraverseDecl(AST->getASTContext().getTranslationUnitDecl());

    /* Collect Clang module map files */
    auto& SrcMgr = AST->getSourceManager();

    MMap = new ModuleMap(AST->getSourceManager(), *Diags, AST->getASTFileLangOpts(),
                            &AST->getTargetInfo(), AST->getHeaderSearch());

    llvm::DenseSet<const clang::DirectoryEntry*> CheckedDirs;
    for (size_t i = 0; i < SrcMgr.loaded_sloc_entry_size(); i++)
    {
        auto SLoc = SrcMgr.getLoadedSLocEntry(i);

        if (SLoc.isExpansion())
            continue;

        auto OrigEntry = SLoc.getFile().getContentCache()->OrigEntry;
        if (!OrigEntry)
            continue;

        auto Dir = OrigEntry->getDir();

        if (CheckedDirs.count(Dir))
            continue;
        CheckedDirs.insert(Dir);

        std::error_code err;
        llvm::sys::fs::directory_iterator DirIt(llvm::Twine(Dir->getName()), err), DirEnd;

        for (; DirIt != DirEnd && !err; DirIt.increment(err))
        {
            auto path = DirIt->path();
            auto extension = llvm::sys::path::extension(path);

            if (extension.equals(".modulemap_d"))
            {
                auto MMapFile = AST->getFileManager().getFile(path);
                assert(MMapFile);

                if (MMap->parseModuleMapFile(MMapFile, false, Dir))
                {
                    ::error(Loc(), "Clang module map '%s/%s' file parsing failed",
                                    MMapFile->getDir(), MMapFile->getName());
                    fatal();
                }
            }
        }
    }

    // Build the builtin type map
    calypso.builtinTypes.build(AST->getASTContext());

    // Initialize the mangling context
    MangleCtx = AST->getASTContext().createMangleContext();
}

void PCH::save()
{
    if (1 || !needSaving) // disabled for now, Clang makes it hard to save a new PCH when an external source like another PCH is loaded by the ASTContext
        return;

    std::error_code EC;
    llvm::raw_fd_ostream OS(pchFilenameNew, EC, llvm::sys::fs::F_None);

    auto& Sysroot = AST->getHeaderSearch().getHeaderSearchOpts().Sysroot;
    auto GenPCH = llvm::make_unique<clang::PCHGenerator>(AST->getPreprocessor(), pchFilenameNew,
                                         nullptr, Sysroot, &OS, true);
    GenPCH->InitializeSema(AST->getSema());
    GenPCH->HandleTranslationUnit(AST->getASTContext());

    needSaving = false;
}

void LangPlugin::GenModSet::parse()
{
    if (parsed)
        return;

    parsed = true;
    clear();

	std::string genFilename(calypso.cachePrefix);
	genFilename += ".gen";

    if (!llvm::sys::fs::exists(genFilename))
        return;

    auto fgenList = fopen(genFilename.c_str(), "r"); // ordered list of headers
    if (!fgenList)
    {
        ::error(Loc(), "Reading .gen file failed");
        fatal();
    }

    char linebuf[MAX_FILENAME_SIZE];
    while (fgets(linebuf, sizeof(linebuf), fgenList) != NULL)
    {
        linebuf[strcspn(linebuf, "\n")] = '\0';
        if (linebuf[0] == '\0')
            continue;

        if (llvm::sys::fs::exists(linebuf))
            insert(strdup(linebuf));
    }

    fclose(fgenList);
}

void LangPlugin::GenModSet::add(::Module *m)
{
    auto& objName = m->objfile->name->str;
    assert(parsed && !count(objName));

	std::string genFilename(calypso.cachePrefix);
	genFilename += ".gen";

    auto fgenList = fopen(genFilename.c_str(), "a");
    if (!fgenList)
    {
        ::error(Loc(), "Writing .gen file failed");
        fatal();
    }

    fprintf(fgenList, "%s\n", objName);
    fclose(fgenList);

    insert(objName);
}

bool LangPlugin::needsCodegen(::Module *m)
{
    assert(isCPP(m));

    genModSet.parse();

    auto& objName = m->objfile->name->str;
    return !genModSet.count(objName);
}

#undef MAX_FILENAME_SIZE

int LangPlugin::doesHandleImport(const utf8_t* tree)
{
    if (strcmp((const char *) tree, "C") == 0
        || strcmp((const char *) tree, "C++") == 0)
        return 0;

    return -1;
}

::Import* LangPlugin::createImport(int treeId, Loc loc, Identifiers* packages,
                                   Identifier* id, Identifier* aliasId, int isstatic)
{
    return new Import(loc,
                packages, id, aliasId, isstatic);
}

int LangPlugin::doesHandleModmap(const utf8_t* lang)
{
    if (strcmp((const char *) lang, "C") == 0
        || strcmp((const char *) lang, "C++") == 0)
        return 0;

    return -1;
}

::Modmap* LangPlugin::createModmap(int langId, Loc loc, Expression* arg)
{
    return new Modmap(loc,
                static_cast<StringExp*>(arg));
}

std::string GetExecutablePath(const char *Argv0) {
  // This just needs to be some symbol in the binary; C++ doesn't
  // allow taking the address of ::main however.
  void *MainAddr = (void*) (intptr_t) GetExecutablePath;
  return llvm::sys::fs::getMainExecutable(Argv0, MainAddr);
}

LangPlugin::LangPlugin()
    : builtinTypes(cpp::builtinTypes),
      declReferencer(cpp::declReferencer)
{
}

void LangPlugin::init(const char *Argv0)
{
    executablePath = GetExecutablePath(Argv0);

    Module::init();
    pch.init();
}

clang::ASTContext& LangPlugin::getASTContext()
{
    return getASTUnit()->getASTContext();
}

bool isCPP(Type* t) { return t->langPlugin() == &calypso; }
bool isCPP(Dsymbol* s) { return s->langPlugin() == &calypso; }

cpp::ClassDeclaration *isDCXX(Dsymbol* s)
{
    auto cd = s->isClassDeclaration();
    assert(cd);

    if (isCPP(cd))
        return nullptr;  // Pure C++ class

    auto base = cd->baseClass;
    while (base && !isCPP(base))
        base = toAggregateBase(base);
    if (!base)
        return nullptr;  // Pure D class

    return static_cast<cpp::ClassDeclaration*>(base);
}

}
