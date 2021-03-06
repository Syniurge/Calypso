// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/cppaggregate.h"
#include "cpp/cpptemplate.h"
#include "cpp/cpptypes.h"
#include "cpp/diagprinter.h"

#include "cpp/ddmdstructor.h"

#include "aggregate.h"
#include "cond.h"
#include "declaration.h"
#include "errors.h"
#include "expression.h"
#include "identifier.h"
#include "id.h"
#include "scope.h"

#include "driver/tool.h"
#include "gen/irstate.h"

#include "clang/AST/DeclTemplate.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Tool.h"
#include "clang/Driver/ToolChain.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/MultiplexConsumer.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Parse/Parser.h"
#include "clang/Sema/Sema.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/Serialization/InMemoryModuleCache.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Target/TargetMachine.h"

#include <fstream>
#include <string>
#include <cctype>
#include <cstring>

extern llvm::cl::opt<bool> linkDefaultLibDebug;
void codegenModules(Modules &modules, bool oneobj);

void log_verbose(const std::string& header, const std::string& msg) {
    // to look aligned with other -v printed lines
    int prefix_width = 9; // TODO: adjust upwards as needed
    fprintf(stderr, "%-*s %s\n", prefix_width, header.c_str(), msg.c_str());
}

namespace opts {
namespace cl = llvm::cl;

cl::list<std::string> cppArgs("cpp-args",
    cl::desc("Clang arguments (space separated) passed during PCH generation. If the list begins with '$', interpret as a single argument"));

cl::opt<bool> cppNoDefaultArgs("cpp-nodefaultargs",
    cl::desc("Do not pass default arguments to Clang (by default \"-c -x c++\" in Unix-like environments, and \"--driver-mode=cl /TP /EHsc\" in MSVC environments)."));

cl::opt<std::string> cppCacheDir("cpp-cachedir",
    cl::desc("Write Calypso cache files to <dir>"),
    cl::value_desc("dir"),
    cl::init(".calypso_cache"));

cl::opt<bool> cppVerboseDiags("cpp-verbosediags",
    cl::desc("Additional Calypso-specific diagnostics (for the time being they may contain spurious errors from failed instantiations that can be ignored)."));

cl::opt<bool> disableRvalueRefParam("disableRvalueRefParam",
    cl::desc("Disable --preview=disableRvalueRefParam (druntime/phobos build and tests)."),
    cl::Hidden);
}

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

LangPlugin calypso;
BuiltinTypes builtinTypes;

RootObject *SpecValue::toTemplateArg(Loc loc)
{
    assert(op || t);
    if (op)
    {
        auto e = new_StringExp(loc, const_cast<char*>(op));
        return expressionSemantic(e, nullptr);
    }
    else
        return t;
}

Identifier *prefixConflictingIdentifier(Identifier *ident)
{
    llvm::SmallString<48> s(u8"ℂ"); // non-ASCII and unavailable on most keyboards, but pretty
    s += llvm::StringRef(ident->toChars(), ident->length());
    return Identifier::idPool(s.c_str(), s.size());
}

Identifier *fromIdentifier(const clang::IdentifierInfo *II)
{
    return Identifier::idPool(II->getNameStart(), II->getLength());
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
                                       clang::OverloadedOperatorKind OO)
{
    std::string fullName(baseIdent->toChars(), baseIdent->length());
    fullName += "_";
    fullName += getOperatorName(OO);

    return Identifier::idPool(fullName.c_str(), fullName.size());
}

static Identifier *getOperatorIdentifier(const clang::FunctionDecl *FD,
                const char *&op, clang::OverloadedOperatorKind OO = clang::OO_None)
{
    if (FD)
        OO = FD->getOverloadedOperator();

    Identifier *opIdent = nullptr;
    bool wrapInTemp = false;

    auto MD = llvm::dyn_cast_or_null<clang::CXXMethodDecl>(FD);
    bool isNonMember = !MD || MD->isStatic();

    unsigned NumParams = 0;
    if (FD) {
        NumParams = FD->getNumParams();
        if (!isNonMember)
            NumParams++;
    }

    if (OO == clang::OO_Call)
        opIdent = Id::call;
    else if(OO == clang::OO_Subscript)
        opIdent = Id::index;
    else
    {
        bool isUnary = NumParams != 2;
        bool isBinary = NumParams != 1;

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
                    // operators without D equivalent
                case clang::OO_Exclaim:
                case clang::OO_Arrow:
                case clang::OO_ArrowStar:
                    opIdent = Id::opUnary;
                    break;
                default:
                    if (FD) {
                        if (opts::cppVerboseDiags)
                            ::warning(Loc(), "Ignoring C++ unary operator %s", clang::getOperatorSpelling(OO));
                        return nullptr;
                    }
            }
        }
        if (isBinary)
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
                    // operators without D equivalent
                case clang::OO_PlusPlus:
                case clang::OO_MinusMinus:
                case clang::OO_Comma:
                case clang::OO_ExclaimEqual:
                    opIdent = Id::opBinary;
                    break;
                case clang::OO_EqualEqual:
                    opIdent = Id::eq;
                    wrapInTemp = false;
                    break;
                case clang::OO_Less:
                case clang::OO_LessEqual:
                case clang::OO_Greater:
                case clang::OO_GreaterEqual:
                    opIdent = fullOperatorMapIdent(Id::cmp, OO);
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
                    if (FD) {
                        if (opts::cppVerboseDiags)
                            ::warning(Loc(), "Ignoring C++ binary operator %s", clang::getOperatorSpelling(OO));
                        return nullptr;
                    }
            }
        }
        // operator new or delete (TODO linking)
    }

    if (!opIdent)
        return nullptr;

    op = wrapInTemp ? getDOperatorSpelling(OO) : nullptr;
    return opIdent;
}

clang::OverloadedOperatorKind toOverloadedOperator(Identifier* ident, const char* arg)
{
    if (ident == Id::opUnary)
    {
        for (auto OO: {clang::OO_Plus, clang::OO_Minus, clang::OO_Star,
                       clang::OO_Tilde, clang::OO_PlusPlus, clang::OO_MinusMinus,
                       clang::OO_Exclaim, clang::OO_Arrow, clang::OO_ArrowStar})
            if (strcmp(arg, getDOperatorSpelling(OO)) == 0)
                return OO;
    }
    else if (ident == Id::opBinary)
    {
        for (auto OO: {clang::OO_Equal, clang::OO_Plus, clang::OO_Minus, clang::OO_Star,
                       clang::OO_Slash, clang::OO_Percent, clang::OO_Caret, clang::OO_Amp,
                       clang::OO_Pipe, clang::OO_Tilde, clang::OO_LessLess,
                       clang::OO_GreaterGreater, clang::OO_PlusPlus, clang::OO_MinusMinus,
                       clang::OO_Comma, clang::OO_ExclaimEqual})
            if (strcmp(arg, getDOperatorSpelling(OO)) == 0)
                return OO;
    }
    else if (ident == Id::opOpAssign)
    {
        for (auto OO: {clang::OO_PlusEqual, clang::OO_MinusEqual, clang::OO_StarEqual,
                       clang::OO_SlashEqual, clang::OO_PercentEqual, clang::OO_CaretEqual,
                       clang::OO_AmpEqual, clang::OO_PipeEqual, clang::OO_LessLessEqual,
                       clang::OO_GreaterGreaterEqual})
            if (strcmp(arg, getDOperatorSpelling(OO)) == 0)
                return OO;
    }
    else if (ident == Id::eq)
    {
        return clang::OO_EqualEqual;
    }
    else if (ident == Id::cmp)
        /*mapOperator(clang::OO_EqualEqual)*/; // FIXME
        // NOTE: other overloaded operators only map to one DeclarationName so don't require
        // special treatment
    else if (ident == Id::_cast)
    {
//         typedef clang::DeclContext::specific_decl_iterator<clang::CXXConversionDecl> Conv_iterator;
//         for (Conv_iterator I(DC->decls_begin()), E(DC->decls_end()); I != E; I++)
//             mapper.dsymAndWrapperForDecl(*I);
//
//         typedef clang::DeclContext::specific_decl_iterator<clang::FunctionTemplateDecl> FuncTemp_iterator;
//         for (FuncTemp_iterator I(DC->decls_begin()), E(DC->decls_end()); I != E; I++)
//             if ((*I)->getDeclName().getNameKind() == clang::DeclarationName::CXXConversionFunctionName)
//                 mapper.dsymAndWrapperForDecl(*I);
    }
    else
        return clang::OO_Spaceship; // i.e "false"

    llvm_unreachable("Unhandled operator");
}

static Identifier *fullConversionMapIdent(Identifier *baseIdent,
                const clang::CXXConversionDecl *D, DeclMapper& mapper)
{
    auto& Context = calypso.getASTContext();

    auto T = D->getConversionType().getDesugaredType(Context);
    auto t = mapper.fromType(T, Loc());

    std::string fullName(baseIdent->toChars(), baseIdent->length());
    fullName += "_";
    if (t->isTypeBasic()) // not too complex, use a readable suffix
    {
        auto TypeQuals = T.getCVRQualifiers();
        if (TypeQuals & clang::Qualifiers::Const) fullName += "const_";
        if (TypeQuals & clang::Qualifiers::Volatile) fullName += "volatile_";
        if (TypeQuals & clang::Qualifiers::Restrict) fullName += "restrict_";
        fullName += t->kind();
    }
    else // generate a name, rare occurrence anyway and only ever matters for virtual conversion operators
    {
        // FIXME: *might* collide
        fullName += T.getAsString();
        fullName.erase(std::remove_if(fullName.begin(), fullName.end(),
            [](char c) { return !isalnum(c); }), fullName.end());
    }

    return Identifier::idPool(fullName.c_str(), fullName.size());
}

static Identifier *getConversionIdentifier(const clang::CXXConversionDecl *D,
                DeclMapper &mapper, Type *&t, clang::QualType T = clang::QualType(),
                bool wantCanonicalType = false)
{
    if (D)
        T = D->getConversionType();

    if (wantCanonicalType)
        T = T.getCanonicalType();

    t = mapper.fromType(T, Loc());
    return Id::_cast;
}

static Identifier *getLambdaIdentifier(const clang::CXXRecordDecl *D)
{
    // NOTE: the mangling number differentiates lambda classes *per* lambda function signature,
    // so both have to enter into account while naming a lambda.
    auto& MangleCtx = calypso.pch.MangleCtx;
    std::string name("__lambda_");

    auto ManglingNumber = D->getLambdaManglingNumber();
    name += std::to_string(ManglingNumber);
    name += "_";

    assert(D->isLambda());

    {
        llvm::raw_string_ostream OS(name);
        MangleCtx->mangleTypeName(
                clang::QualType(D->getLambdaCallOperator()->getFunctionType(), 0), OS);
    }

    return Identifier::idPool(name.c_str(), name.size());
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

Identifier *getIdentifierOrNull(const clang::NamedDecl *D, SpecValue *spec, bool useCanonicalType)
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
        return getConversionIdentifier(Conv, spec->mapper, spec->t,
                                       clang::QualType(), useCanonicalType);
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
    else if (auto Tag = dyn_cast<clang::TagDecl>(D)) {
        if (auto Typedef = Tag->getTypedefNameForAnonDecl())
            II = Typedef->getIdentifier();
        else if (auto Record = dyn_cast<clang::CXXRecordDecl>(D))
            if (Record->isLambda())
                if (auto lambdaIdent = getLambdaIdentifier(Record))
                    return lambdaIdent;
    }

    if (!II)
        return nullptr;

    auto ident = fromIdentifier(II);
    bool needsPrefixing = false;

    auto ParentTag = dyn_cast<clang::TagDecl>(D->getDeclContext());
    if (II && isa<clang::TagDecl>(D) && ParentTag)
    {
        auto R = ParentTag->lookup(D->getDeclName());
        for (auto Match: R)
        {
            if (!isa<clang::ValueDecl>(*Match))
                continue;

            // The nested tag shares a common name with a field, mapping it directly would cause a symbol conflict
            needsPrefixing = true;
            break;
        }
    }

    if (needsPrefixing)
        ident = prefixConflictingIdentifier(ident);

    return ident;
}

Identifier *idPool(const char *s)
{
    return Identifier::idPool(s, std::strlen(s));
}

Identifier *getIdentifier(const clang::NamedDecl *D, SpecValue *spec, bool useCanonicalType)
{
    auto result = getIdentifierOrNull(D, spec, useCanonicalType);
    assert(result);

    return result;
}

Identifier *getExtendedIdentifierOrNull(const clang::NamedDecl *D,
                                  DeclMapper &mapper)
{
    SpecValue spec(mapper);
    auto ident = getIdentifierOrNull(D, &spec);
    if (!ident)
        return nullptr;

    auto FD = dyn_cast<clang::FunctionDecl>(D);
    if (spec.op && FD)
        ident = fullOperatorMapIdent(ident,
                        FD->getOverloadedOperator());
    else if (spec.t)
        ident = fullConversionMapIdent(ident,
                        cast<clang::CXXConversionDecl>(D), mapper);

    return ident;
}

Identifier *getExtendedIdentifier(const clang::NamedDecl *D,
                                  DeclMapper &mapper)
{
    auto result = getExtendedIdentifierOrNull(D, mapper);
    assert(result);

    return result;
}

RootObject *getIdentOrTempinst(Loc loc, const clang::DeclarationName N,
                               DeclMapper &mapper)
{
    SpecValue spec(mapper);
    auto ident = fromDeclarationName(N, &spec);
    if (!ident)
        return nullptr;

    if (spec)
    {
        auto tiargs = new Objects;
        tiargs->push(spec.toTemplateArg(loc));
        auto tempinst = new cpp::TemplateInstance(loc, ident, tiargs);
        return tempinst;
    }
    else
        return ident;
}

// ***** //

clang::IdentifierInfo* LangPlugin::toIdentifierInfo(Identifier* ident)
{
    auto& II = IIMap[ident];

    if (!II)
    {
        const char prefix[] = u8"ℂ";
        const size_t prefixLength = sizeof(prefix)-1;

        auto& Ctx = getASTContext();

        bool prefixed = strncmp(ident->toChars(), prefix, prefixLength) == 0;

        const char* str = !prefixed ? ident->toChars() : ident->toChars() + prefixLength;
        size_t len = ident->length();
        if (prefixed)
            len -= prefixLength;

        II = &Ctx.Idents.get(llvm::StringRef(str, len));
    }

    return II;
}

clang::DeclarationName LangPlugin::toDeclarationName(Identifier* ident, const clang::RecordDecl* RD)
{
    auto& DeclarationNames = getASTContext().DeclarationNames;

    if (ident == Id::call)
        return DeclarationNames.getCXXOperatorName(clang::OO_Call);
    else if (ident == Id::index)
        return DeclarationNames.getCXXOperatorName(clang::OO_Subscript);
    else if (ident == Id::eq)
        return DeclarationNames.getCXXOperatorName(clang::OO_EqualEqual);
    else if (ident == Id::assign)
        return DeclarationNames.getCXXOperatorName(clang::OO_Equal);

    if (RD)
    {
        auto RecordTy = getASTContext().getCanonicalType(
                    clang::QualType(RD->getTypeForDecl(), 0));

        if (ident == Id::ctor)
            return DeclarationNames.getCXXConstructorName(RecordTy);
        else if (ident == Id::dtor)
            return DeclarationNames.getCXXDestructorName(RecordTy);
    }

    return clang::DeclarationName(toIdentifierInfo(ident));
}

// ===== //

Loc fromLoc(clang::SourceLocation L)
{
    auto& SrcMgr = calypso.getSourceManager();
    Loc loc;

    if (L.isInvalid())
        return loc;

    if (L.isMacroID())
        L = SrcMgr.getExpansionLoc(L);

    auto S = SrcMgr.getFilename(L);
    loc.filename = S.data();
    assert(!S.data() || *(S.data() + S.size()) == '\0'); // TEMPORARY assert to confirm that StringRef isn't needed anymore
    loc.linnum = SrcMgr.getSpellingLineNumber(L);

    return loc;
}

/***********************/

// see CodeGenModule::getMangledName()
void LangPlugin::mangle(const clang::NamedDecl *ND, std::string& str)
{
    auto MangleCtx = pch.MangleCtx;

    llvm::SmallString<128> Buffer;
    llvm::StringRef Str;
    llvm::raw_svector_ostream Out(Buffer);

    if (auto RD = dyn_cast<clang::RecordDecl>(ND))
    {
        MangleCtx->mangleCXXRTTI(clang::QualType(RD->getTypeForDecl(), 0), Out);
        Str = Out.str();
    }
    else if (MangleCtx->shouldMangleDeclName(ND))
    {
        if (const auto *D = dyn_cast<clang::CXXConstructorDecl>(ND))
            MangleCtx->mangleCXXCtor(D, clang::Ctor_Complete, Out);
        else if (const auto *D = dyn_cast<clang::CXXDestructorDecl>(ND))
            MangleCtx->mangleCXXDtor(D, clang::Dtor_Complete, Out);
        else
            MangleCtx->mangleName(ND, Out);
        Str = Out.str();
    }
    else
    {
        auto II = ND->getIdentifier();
        assert(II && "Attempt to mangle unnamed decl.");
        Str = II->getName();
    }

    Str.str().swap(str);
}

/***********************/

void PCH::init()
{
    stubHeader = calypso.getCacheFilename(".h");
    pchFilename = calypso.getCacheFilename(".h.pch");
    pchFileList = calypso.getCacheFilename(".list");

    readPchFileList();

    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> DiagOpts(new clang::DiagnosticOptions);
    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> DiagID(new clang::DiagnosticIDs);
    DiagClient = new DiagnosticPrinter(llvm::errs(), &*DiagOpts);
    DiagClient->muted = !opts::cppVerboseDiags;
    Diags = new clang::DiagnosticsEngine(DiagID,
                                         &*DiagOpts, DiagClient);
}

void PCH::add(const char* header, ::Module *from)
{
    // First check whether the path points towards a file relative to the module directory or a header from -I options or system include dirs
    // In the first case we need to make the path absolute since the calypso_cache directory is most of the time different.
    if (header[0] != '<')
    {
        using namespace llvm::sys::fs;
        using namespace llvm::sys::path;

        auto headerFn = new llvm::SmallString<64>(
                            parent_path(from->srcfile.toChars()));
        append(*headerFn, llvm::StringRef(header));

        file_status result;
        status(llvm::Twine(*headerFn), result);
        if (is_regular_file(result))
        {
            make_absolute(*headerFn);
            headerFn->push_back('\0');
            header = headerFn->c_str();
        }
    }

    for (unsigned i = 0; i < headers.dim; i++)
        if (strcmp(header, headers[i]) == 0)
            return;

    headers.push(header);
}

void PCH::readPchFileList()
{
    // NOTE: It is much simpler and faster to maintain the cppmap'd filenames
    // inside a file separate from the PCH than to query the headers known to the PCH.
    // SLoc are being loaded lazily, and there may be thousands of headers in the
    // control block.
    // A downside is that if a header is already included by another header in
    // the PCH, a full reload will still get triggered.

    std::ifstream fpchFileList(pchFileList);
    if (!fpchFileList)
        return;

    char filename[4096];

    while (!(fpchFileList.getline(filename, sizeof(filename)).rdstate())) {
        pchFileListSet.insert(filename);
        if (fpchFileList.rdstate() & std::istream::failbit) {
            ::error(Loc(), "Error reading '%s'", pchFileList.c_str());
            fatal();
        }
    }
}

void PCH::writePchFileList()
{
    std::ofstream fpchFileList(pchFileList, std::ios::trunc);
    if (!fpchFileList) {
        ::error(Loc(), "Error creating '%s'", pchFileList.c_str());
        return;
    }

    for (auto& headerRealPath: headerPaths)
        fpchFileList << headerRealPath.c_str() << std::endl;
}

void PCH::loadFirstHeaders(bool includePCH)
{
    // Re-emit a 'calypso_cache.h' stub
    // NOTE: the stub must not be populated by new headers, or else the file timestamp changes
    // while the PCH written later will record the old timestamp provided by FileManager.
    if (!includePCH)
    {
        std::ofstream fmono(stubHeader);
        if (!fmono) {
            ::error(Loc(), "'%s' couldn't be created", stubHeader.c_str());
            fatal();
        }
        fmono << "/* Precompiled header 'stub' */\n";
    }

    std::unique_ptr<clang::driver::Compilation> C(calypso.buildClangCompilation());

    // We use a trick from clang-interpreter to extract -cc1 flags from "puny human" flags
    // We expect to get back at least one command job, if we didn't something
    // failed. Extract the first job from the compilation.
    const clang::driver::JobList &Jobs = C->getJobs();

    if (Jobs.empty() || !isa<clang::driver::Command>(*Jobs.begin())) {
        ::error(Loc(), "Arguments passed to the C++ compiler with --cpp-args must result in at least one compilation job");
        fatal();
    }

    const clang::driver::Command &Cmd = cast<clang::driver::Command>(*Jobs.begin());
    assert(llvm::StringRef(Cmd.getCreator().getName()) == "clang");

    DiagClient->muted = false;

    // Initialize a compiler invocation object from the clang (-cc1) arguments.
    auto CCArgs = Cmd.getArguments();
    if (includePCH) {
        CCArgs.push_back("-include-pch");
        CCArgs.push_back(pchFilename.c_str());
        DiagClient->muted = !opts::cppVerboseDiags;
    }

    auto CI = std::make_shared<clang::CompilerInvocation>();
    clang::CompilerInvocation::CreateFromArgs(*CI, CCArgs.begin(), CCArgs.end(), *Diags);

    // Initialize most of Clang's state
    llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> OverlayFileSystem(
        new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem()));
    auto Files = new clang::FileManager(clang::FileSystemOptions(), OverlayFileSystem);

    PCHContainerOps.reset(new clang::PCHContainerOperations);

    AST = clang::ASTUnit::LoadFromCompilerInvocation(CI, PCHContainerOps, Diags, Files, false,
                                                     clang::CaptureDiagsKind::None, false,
                                                     clang::TU_Complete, false, false, false);

    DiagClient->muted = !opts::cppVerboseDiags;

    if (!AST) {
        if (!includePCH)
            fatal();

        if (opts::cppVerboseDiags)
            log_verbose("calypso", "out-of-date PCH, reparsing every C/C++ header");

        return; // PCH may be stale, trigger a reparsing
    }

    auto& FileMgr = AST->getFileManager();
    auto& SrcMgr = AST->getSourceManager();

    auto stubHeaderFileEntry = FileMgr.getFile(stubHeader);
    stubHeaderFileID = SrcMgr.translateFile(stubHeaderFileEntry);

    AST->getSema();
    Diags->getClient()->BeginSourceFile(AST->getLangOpts(), &AST->getPreprocessor());

    initializeParser();

    if (includePCH) {
        const clang::DirectoryLookup *CurDir;

        // Check whether every cppmap'd header is present within the PCH
        for (auto header: headers) {
            auto fileEntry = lookupHeader(header, CurDir);

            llvm::SmallString<96> RealPath;
            llvm::sys::fs::real_path(fileEntry->tryGetRealPathName(), RealPath);

            if (pchFileListSet.count(RealPath)) {
                headerPaths.push_back(std::move(RealPath));
                continue; // already known to the PCH
            }

            // else, retrigger a full reload
            headerPaths.clear();
            AST.reset();
            return;
        }

        nextHeader = headers.size();
    }
    else
    {
        loadNewHeaders();

        /* Mark every C++ module object file dirty */

        auto genListFilename = calypso.getCacheFilename(".gen");
        llvm::sys::fs::remove(genListFilename, true);
    }
}

void PCH::loadNewHeaders()
{
    for (; nextHeader < headers.size(); nextHeader++)
        if (!loadHeader(headers[nextHeader]))
            fatal();
}

bool PCH::loadHeader(const char* header)
{
    // First check whether header is already present within the PCH
    // FIXME: this doesn't support headers that are "configurable" and meant to be included more than once

    const clang::DirectoryLookup *CurDir;

    auto File = lookupHeader(header, CurDir);
    if (!File) {
        ::error(Loc(), "'%s' file not found", header);
        return false;
    }

    llvm::SmallString<96> RealPath;
    llvm::sys::fs::real_path(File->tryGetRealPathName(), RealPath);
    headerPaths.push_back(std::move(RealPath));

    if (AST->getASTContext().getExternalSource() &&
                pchFileListSet.count(File->tryGetRealPathName()))
        return true; // already known to the PCH

    needSaving = true;

    auto& SrcMgr = AST->getSourceManager();
    auto& PP = AST->getPreprocessor();
    auto& S = AST->getSema();
    assert(S.OpaqueParser);
    auto& P = *static_cast<clang::Parser*>(S.OpaqueParser);
    auto* Consumer = &S.getASTConsumer();

    DiagClient->muted = false;

    assert(P.getCurScope());
    restoreTUScope();

    auto IncludeLoc = SrcMgr.getLocForStartOfFile(stubHeaderFileID);

    auto FileCharacter = PP.getHeaderSearchInfo().getFileDirFlavor(File);
    auto FID = SrcMgr.translateFile(File);
    if (!FID.isValid())
        FID = SrcMgr.createFileID(File, IncludeLoc, FileCharacter);

    ++CurDir; // it is safe to increment CurDir for #include_next directives
              // CurDir is only used for those, and -frewrite-includes which has no use here

    if (PP.EnterSourceFile(FID, CurDir, IncludeLoc))
        fatal();
    P.ConsumeToken();

    clang::Parser::DeclGroupPtrTy ADecl;
    for (bool AtEOF = P.ParseTopLevelDecl(ADecl); !AtEOF;
            AtEOF = P.ParseTopLevelDecl(ADecl)) {
        if (ADecl && !Consumer->HandleTopLevelDecl(ADecl.get()))
            fatal();
    }

    for (clang::Decl *D : S.WeakTopLevelDecls())
        Consumer->HandleTopLevelDecl(clang::DeclGroupRef(D));

    restoreTUScope(); // restore TUScope and CurContext for later instantiations

    DiagClient->muted = !opts::cppVerboseDiags;

    if (Diags->hasErrorOccurred())
    {
        ::error(Loc(), "Invalid C/C++ header(s)");
        return false;
    }

    return true;
}

const clang::FileEntry* PCH::lookupHeader(const char* header, const clang::DirectoryLookup*& CurDir)
{
    auto& SrcMgr = AST->getSourceManager();
    auto& PP = AST->getPreprocessor();

    bool IsMapped;
    llvm::ArrayRef<std::pair<const clang::FileEntry *, const clang::DirectoryEntry *>> Includers;

    bool isAngled = header[0] == '<';
    llvm::StringRef headerStr(header);

    auto IncludeLoc = SrcMgr.getLocForStartOfFile(stubHeaderFileID);

    return PP.getHeaderSearchInfo().LookupFile(
                isAngled ? headerStr.substr(1, headerStr.size()-2) : headerStr,
                IncludeLoc, isAngled, nullptr, CurDir, Includers,
                /*SearchPath*/ nullptr, /*RelativePath*/ nullptr, nullptr,
                nullptr, &IsMapped, /*IsFrameworkFound*/ nullptr, false, false);
}

void PCH::initializeParser()
{
    auto& PP = AST->getPreprocessor();
    auto& S = AST->getSema();

    // Recreate a Parser and CurScope if necessary
    if (!S.OpaqueParser)
        S.OpaqueParser = new clang::Parser(PP, S, /*SkipFunctionBodies=*/ false);

    auto& P = *static_cast<clang::Parser*>(S.OpaqueParser);
    if (!P.getCurScope())
        P.EnterScope(clang::Scope::DeclScope);

//     PP.enableIncrementalProcessing();

    // MSVC: the simplest way to set LangOpts.IsHeaderFile to true is to do this..
    // And it needs to be true for #pragma once headers.
    const_cast<clang::LangOptions&>(PP.getLangOpts()).IsHeaderFile = true;
}

void PCH::restoreTUScope()
{
    auto& S = AST->getSema();
    assert(S.OpaqueParser);
    auto& P = *static_cast<clang::Parser*>(S.OpaqueParser);

    if (!S.TUScope) {
        S.CurContext = nullptr;
        S.ActOnTranslationUnitScope(P.getCurScope());
    }
}

void PCH::update()
{
    if (nextHeader >= headers.size())
        return;

    if (AST) {
        loadNewHeaders();
        return;
    }

    bool needHeadersReload = false;

    auto CheckCacheFile = [&] (std::string& fn) {
        using namespace llvm::sys::fs;

        file_status result;
        status(fn, result);
        if (is_directory(result)) {
            ::error(Loc(), "%s is a directory\n", fn.c_str());
            fatal();
        }

        if (!exists(result))
            needHeadersReload = true;
    };

    CheckCacheFile(stubHeader);
    CheckCacheFile(pchFilename);

    if (!needHeadersReload)
        // Give the existing PCH a try
        loadFirstHeaders(true);

    if (!AST)
        // Reparse the cppmap'd header files
        loadFirstHeaders(false);

//     // Since the out-of-dateness of headers are checked lazily for most of them, it might only be detected
//     // by walking through all the SLoc entries. If an error occurred start over and trigger a loadFirstHeaders.
//     if (Diags->hasErrorOccurred())
//     {
//         Diags->Reset();
//
//         needHeadersReload = true;
//         return update();
//     }

    // Build the builtin type map
    calypso.builtinTypes.build(AST->getASTContext());

    // Initialize the mangling context
    MangleCtx = AST->getASTContext().createMangleContext();
}

void PCH::save()
{
    if (!needSaving)
        return;

    // FIXME: Clang makes it hard to save a new PCH when an external source like another PCH is loaded by the ASTContext (later NOTE: still true?)
    if (AST->getASTContext().getExternalSource() != nullptr) {
        // The PCH doesn't contain every cppmap'd header, so delete
        // the calypso_cache.h stub to trigger a full reparsing next build
        llvm::sys::fs::remove(stubHeader, true);
        return;
    }

    auto& PP = AST->getPreprocessor();

    std::error_code EC;
    std::unique_ptr<llvm::raw_fd_ostream> OS(
                new llvm::raw_fd_ostream(pchFilename, EC, llvm::sys::fs::F_None));

    auto& Sysroot = PP.getHeaderSearchInfo().getHeaderSearchOpts().Sysroot;
    auto Buffer = std::make_shared<clang::PCHBuffer>();
    auto *Writer = PCHContainerOps->getWriterOrNull("raw");

    clang::InMemoryModuleCache ModuleCache;
    auto GenPCH = new clang::PCHGenerator(PP, ModuleCache, pchFilename, Sysroot, Buffer,
                                          llvm::ArrayRef<std::shared_ptr<clang::ModuleFileExtension>>(),
                                          /* AllowASTWithErrors = */ true);
    GenPCH->InitializeSema(AST->getSema());

    std::vector<std::unique_ptr<clang::ASTConsumer>> Consumers;
    Consumers.push_back(std::unique_ptr<clang::ASTConsumer>(GenPCH));
    Consumers.push_back(Writer->CreatePCHContainerGenerator(
        *static_cast<clang::CompilerInstance*>(nullptr), stubHeader, pchFilename, std::move(OS), Buffer));

    auto Mutiplex = llvm::make_unique<clang::MultiplexConsumer>(std::move(Consumers));
    Mutiplex->HandleTranslationUnit(AST->getASTContext());

    writePchFileList();

    needSaving = false;
}

void LangPlugin::codegenModules()
{
    for (auto& pair: cpp::Module::allCppModules) {
        auto m = pair.second;

        for (auto D: m->instantiatedDecls)
        {
            std::string MangledName;
            calypso.mangle(cast<clang::NamedDecl>(D), MangledName);

            if (!m->emittedSymbols.count(MangledName))
            {
                if (!m->needGen) {
                    m->needGen = true;
                    m->emittedSymbols.clear();
                }
                m->emittedSymbols.insert(MangledName);
            }
        }

        m->checkAndAddOutputFile(m->objfile);
        global.params.objfiles.push(m->objfile.toChars());
    }
    ::codegenModules(cpp::Module::amodules_cpp, false);
}

bool LangPlugin::needsCodegen(::Module *m)
{
    assert(isCPP(m));
    auto c_m = static_cast<cpp::Module*>(m);

    return c_m->needGen;
}

int LangPlugin::doesHandleImport(const char* lang)
{
    if (strcmp((const char *) lang, "C") == 0
        || strcmp((const char *) lang, "C++") == 0)
        return 0;

    return -1;
}

::Import* LangPlugin::createImport(int langId, Loc loc, Identifiers* packages,
                                   Identifier* id, Identifier* aliasId, int isstatic)
{
    return new Import(loc,
                packages, id, aliasId, isstatic);
}

static bool parseStringExp(Expression *e, const char *&res) {
    e = e->optimize(WANTvalue);
    if (e->op != TOKstring) {
        return false;
    }
    auto s = static_cast<StringExp *>(e);
    res = s->toStringz();
    return true;
}

int LangPlugin::getPragma(Scope* sc, PragmaDeclaration* decl)
{
    if (decl->ident == id_cppmap) {
        Expressions *args = decl->args;
        Expression *expr =
            (args && args->dim > 0) ? expressionSemantic((*args)[0], sc) : nullptr;

        const char *arg1str;
        if (!args || args->dim != 1 || !parseStringExp(expr, arg1str)) {
            error(decl->loc, "must specify a C/C++ header (<xyz.h> for system includes)");
            fatal();
        }

        pch.add(arg1str, sc->_module);
        return 1;
    }
    return 0;
}

void LangPlugin::pragmaSemantic(Scope* sc, PragmaDeclaration* decl)
{
}

bool LangPlugin::checkAccess(Loc loc, Scope* sc, ::Package* p)
{
    return false; // always accessible
}

void LangPlugin::adjustLinkerArgs(std::vector<std::string>& args)
{
    if (!getASTUnit())
        return;

    auto argsFindSubstr = [&] (const char* substr) {
        return std::find_if(args.begin(), args.end(),
                [&] (const std::string& str) { return str.find(substr) != std::string::npos; });
    };

    if (argsFindSubstr("calypso-ldc") == args.end()) {
        // Insert -lcalypso-ldc before -ldruntime-ldc if not already passed yet (e.g to link against the -shared version)
        auto it_druntime = argsFindSubstr("druntime-ldc"); // FIXME: too heuristical
        assert(it_druntime != args.end());

        std::string calypso_ldc = "-lcalypso-ldc",
            calypso_ldc_debug = "-lcalypso-ldc-debug";

        if (global.params.targetTriple->isWindowsMSVCEnvironment()) {
            calypso_ldc = calypso_ldc.substr(2) + ".lib";
            calypso_ldc_debug = calypso_ldc_debug.substr(2) + ".lib";
        }

        if (linkDefaultLibDebug)
            args.insert(it_druntime, calypso_ldc_debug);
        else
            args.insert(it_druntime, calypso_ldc);
    }

    if (global.params.targetTriple->isWindowsMSVCEnvironment()) {
        // auto it_kernel32 = std::find(args.begin(), args.end(), "kernel32.lib");
        // NOTE: the choice of the MSVC CRT library (LIBCPMT.LIB, MSVCPRT.LIB, etc.) should be left either to the linker
        // (if linking against 3rd-party C++ libraries, link.exe picks up the runtime library expected by that library)
        // or to the user.
        // The right -D flags must also be manually passed: -cpp-args -D_DEBUG, -DMD(d), -DMT(d)
    } else {
        // Insert -lstdc++ or -lc++
        const char* cxxstdlib = (cxxStdlibType ==
            clang::driver::ToolChain::CST_Libcxx) ? "-lc++" : "-lstdc++";

        auto it_pthread = std::find(args.begin(), args.end(), "-lpthread");
        auto it_m = std::find(args.begin(), args.end(), "-lm");

        if (it_pthread != args.end())
            args.insert(it_pthread, cxxstdlib);
        else if (it_m != args.end())
            args.insert(it_m, cxxstdlib); // Solaris
    }
}

std::string GetExecutablePath(const char *Argv0) {
  // This just needs to be some symbol in the binary; C++ doesn't
  // allow taking the address of ::main however.
  void *MainAddr = (void*) (intptr_t) GetExecutablePath;
  return llvm::sys::fs::getMainExecutable(Argv0, MainAddr);
}

LangPlugin::LangPlugin()
    : builtinTypes(cpp::builtinTypes)
{
}

void LangPlugin::_init()
{
    if (!llvm::sys::fs::exists(opts::cppCacheDir))
        llvm::sys::fs::create_directory(opts::cppCacheDir);
    if (!llvm::sys::fs::is_directory(opts::cppCacheDir)) {
        ::error(Loc(), "%s isn't a directory", opts::cppCacheDir.c_str());
        fatal();
    }

    if (!opts::disableRvalueRefParam)
        global.params.rvalueRefParam = true;

    id_cppmap = idPool("cppmap");

    id_cpp = idPool("cpp");
    id_core = idPool("core");
    id__ = idPool("_");
    id_Scpp = idPool(u8"ℂcpp");

    id_isCpp = idPool("isCpp");
    id_getCppVirtualIndex = idPool("getCppVirtualIndex");
    id_getBaseOffset = idPool("getBaseOffset");
    id_getMemberPointerExtraSlots = idPool("getMemberPointerExtraSlots");

    id___cpp_type_info_ptr = idPool("__calypso_type_info_ptr"); // FIXME
    id_cpp_member_ptr = idPool("__cpp_member_ptr");

    id_op = idPool("op");
    id_type = idPool("type");

    id___va_list_tag = idPool("__va_list_tag");
    id___NSConstantString_tag = idPool("__NSConstantString_tag");

    pch.init();

    auto TargetFS = gTargetMachine->getTargetFeatureString();
    if (!TargetFS.empty()) { // always empty for MSVC
        llvm::SmallVector<llvm::StringRef, 1> AttrFeatures;
        TargetFS.split(AttrFeatures, ",");

        for (auto &Feature : AttrFeatures) {
            Feature = Feature.trim();

            if (Feature.startswith("-"))
                continue;

            TargetFeatures.insert(Feature);
        }
    }

    // The driver doesn't do anything except computing the flags and informing us of the toolchain's C++ standard lib.
    // NOTE: Calypso doesn't call any clang executable
    clangArgv.push_back("clang");

    // eg: handle line=" -Ifoo -v " => "-Ifoo", "-v"
    static std::vector<std::string> args;
    for (auto& line: opts::cppArgs) {
        std::string arg;
        if (line.empty())
            continue;

        // NOTE: an alternative would be to use "foo\ bar" for escaping space, but it has other drawbacks, eg for windows, or for escaping '\' itself; also it adds complexity on user shell command. This seems simpler.

        // TODO: is another character more standard and cross-platform (and less susceptible to bash substitution)?
        char single_arg='$';

        if (line[0] == single_arg) {
            if (line.size() > 1)
                args.push_back(line.substr(1));
            continue;
        }

        for (char c: line)
            if (isspace(c)) {
                if (!arg.empty()) {
                    args.push_back(arg);
                    arg.clear();
                }
            } else
                arg.push_back(c);

        if (!arg.empty())
            args.push_back(arg);
    }
    for (const auto& argi: args)
        clangArgv.push_back(argi.c_str());

    if (!opts::cppNoDefaultArgs)  {
        if (!global.params.targetTriple->isWindowsMSVCEnvironment()) {
            clangArgv.push_back("-c");
            clangArgv.push_back("-x");
            clangArgv.push_back("c++-header");
        } else {
            clangArgv.push_back("--driver-mode=cl");
            clangArgv.push_back("/TP");
            clangArgv.push_back("/EHsc");
        }
    }

    if (!global.params.targetTriple->isWindowsMSVCEnvironment()) {
        clangArgv.push_back("-");

        // Build a dummy compilation to extract the standard library type
        std::unique_ptr<clang::driver::Compilation> C(buildClangCompilation());
        cxxStdlibType = C->getDefaultToolChain().GetCXXStdlibType(C->getArgs());

        switch (cxxStdlibType)
        {
            case clang::driver::ToolChain::CST_Libcxx:
                VersionCondition::addPredefinedGlobalIdent("CppStdLib_libcxx");
                break;
            case clang::driver::ToolChain::CST_Libstdcxx:
                VersionCondition::addPredefinedGlobalIdent("CppStdLib_libstdcxx");
                break;
            default:
                break;
        }

        clangArgv.pop_back();
    }

    clangArgv.push_back(pch.stubHeader.c_str());

    if (global.params.verbose) {
        std::string msg = "driver args: ";
        for (std::string arg : clangArgv) {
            for (auto i = std::find(arg.begin(), arg.end(), ' '); i != arg.end();
                        i = std::find(i, arg.end(), ' '))
                i = ++++arg.insert(i, '\\');
            msg += arg;
            msg += " ";
        }
        log_verbose("calypso", msg);
    }
}

clang::driver::Compilation* LangPlugin::buildClangCompilation()
{
    std::string TripleStr = llvm::sys::getProcessTriple();
    llvm::Triple T(TripleStr);

    clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> CC1DiagOpts(new clang::DiagnosticOptions);
    clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> CC1DiagID(new clang::DiagnosticIDs);
    auto CC1DiagClient = new clang::TextDiagnosticPrinter(llvm::errs(), &*CC1DiagOpts);
    clang::IntrusiveRefCntPtr<clang::DiagnosticsEngine> CC1Diags = new clang::DiagnosticsEngine(CC1DiagID,
                                        &*CC1DiagOpts, CC1DiagClient);

    // FIXME Clang 4+: We currently reconstruct a Driver every time we want a new compilation because things break if we don't, but this shoudn't be so
    TheDriver.reset(new clang::driver::Driver(GetExecutablePath(Argv0), T.str(), *CC1Diags));
    TheDriver->setTitle("Calypso");

    auto C = TheDriver->BuildCompilation(clangArgv);
    assert(C);
    return C;
}

clang::ASTContext& LangPlugin::getASTContext()
{
    return getASTUnit()->getASTContext();
}

clang::Sema& LangPlugin::getSema()
{
    return getASTUnit()->getSema();
}

clang::DiagnosticsEngine& LangPlugin::getDiagnostics()
{
    return getASTUnit()->getDiagnostics();
}

clang::Preprocessor& LangPlugin::getPreprocessor()
{
    return getASTUnit()->getPreprocessor();
}

clang::SourceManager& LangPlugin::getSourceManager()
{
    return getASTUnit()->getSourceManager();
}

clang::FileManager& LangPlugin::getFileManager()
{
    return getASTUnit()->getFileManager();
}

std::string LangPlugin::getCacheFilename(const char *suffix)
{
    using namespace llvm::sys::path;

    std::string fn(calypso.cachePrefix);
    llvm::SmallString<64> fullpath(opts::cppCacheDir);

    if (suffix)
        fn += suffix;
    append(fullpath, fn);

    return fullpath.str().str();
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
