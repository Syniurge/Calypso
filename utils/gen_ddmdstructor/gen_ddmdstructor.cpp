 /*
    Contributed by Elie 'Syniurge' Morisse (syniurge@gmail.com)
*/

 // Build with clang++ -ogen_ddmdstructor -std=c++11 -I/usr/lib/llvm-3.9/include -lclang-3.9 gen_ddmdstructor.cpp
 // Run with: ./gen_ddmdstructor  -DIN_LLVM -I../../../ldc/ddmd -I../../../ldc/ddmd/root -I/usr/include/clang/3.9.1/include ddmd_headers.h
 //   (over a LDC copy, and not inside Calypso where some C++ constructors were commented out)

 // NOTE: choosing libclang was a mistake once again, it's still terribly lacking: no access to default function arguments, isPolymorphic/isAbstractClass not exposed
 // But this tool is already obsolete since constructors have been removed from ddmd/*.h files in later releases. It needs to be replaced with a tool based on DMD.

#include <iostream>
#include <fstream>
#include <regex>
#include <vector>
#include <string.h>
#include <clang-c/Index.h>
#include <boost/algorithm/string/replace.hpp>

using std::cout;
using std::cerr;

namespace {
    struct CXStringEx : public CXString
    {
        CXStringEx () {}
        CXStringEx (const CXString s) { data = s.data; private_flags = s.private_flags; }
        ~CXStringEx () { clang_disposeString(*this); }

        CXStringEx& operator= (const CXString& s) { data = s.data; private_flags = s.private_flags; return *this; }
    };

    struct VisitContext
    {
        std::ofstream *out;
        bool forD = false;
    };

    std::string cursorFilenameLocation(CXCursor cursor)
    {
        CXFile cursorFile;
        clang_getSpellingLocation (clang_getCursorLocation (cursor), &cursorFile, nullptr, nullptr, nullptr);

        CXStringEx cursorFilename = clang_getFileName (cursorFile);
        return std::string(clang_getCString(cursorFilename));
    }

    CXChildVisitResult CursorVisitor_isPolymorphic(CXCursor cursor, CXCursor, CXClientData client_data)
    {
        bool& result = *(bool*) client_data;

        switch (clang_getCursorKind (cursor))
        {
            case CXCursor_CXXBaseSpecifier:
                result = true;
                return CXChildVisit_Break;
            case CXCursor_Destructor:
            case CXCursor_CXXMethod:
                if (clang_CXXMethod_isVirtual(cursor))
                    result = true;
                break;
            default:
                break;
        }
        return CXChildVisit_Continue;
    }

    bool isPolymorphic(CXCursor cursor)
    {
          bool result = false;
          clang_visitChildren(cursor, &CursorVisitor_isPolymorphic, &result);
          return result;
    }

    bool isPolymorphic(CXType type)
    {
          CXCursor cursor = clang_getTypeDeclaration(type);
          if (clang_isInvalid(cursor.kind))
              return false;
          return isPolymorphic(cursor);
    }

    // Not accurate but enough for DDMD. What a waste of time, how was this not exposed to libclang yet?..
    CXChildVisitResult CursorVisitor_isAbstractClass(CXCursor cursor, CXCursor, CXClientData client_data)
    {
        bool& result = *(bool*) client_data;
        switch (clang_getCursorKind (cursor))
        {
            case CXCursor_CXXMethod:
                if (clang_CXXMethod_isPureVirtual(cursor))
                    result = true;
                break;
            default:
                break;
        }
        return CXChildVisit_Continue;
    }

    bool isAbstractClass(CXCursor cursor)
    {
          bool result = false;
          clang_visitChildren(cursor, &CursorVisitor_isAbstractClass, &result);
          return result;
    }

    const char * const typeDReplace[][2] = {
        { "unsigned char", "ubyte" },
        { "unsigned short", "ushort" },
        { "unsigned int", "uint" },
        { "unsigned long long", "ulong" },
        { "unsigned", "uint" },
        { "utf8_t", "char" },
    };

    std::string toDSpelling(CXType type)
    {
        std::string result;
        auto pointee = clang_getPointeeType(type);
        if (pointee.kind != CXType_Invalid && isPolymorphic(pointee))
            type = pointee;
        result = clang_getCString(clang_getTypeSpelling(type));
        for (auto& rep: typeDReplace)
            result = boost::replace_all_copy(result, rep[0], rep[1]);
        std::regex e("const ([^\\s*]+)");
        result = std::regex_replace (result, e,"const ($1)");
        return result;
    }

    const char * const identDKeywords[] = {
        "body",
        "string",
    };

    std::string toDSpelling(CXCursor cursor)
    {
        std::string result = clang_getCString(clang_getCursorSpelling(cursor));
        for (auto& s: identDKeywords)
            if (result.compare(s) == 0)
                result.insert(0, "_");
        return result;
    }

    CXChildVisitResult CursorVisitor_Aggregate(CXCursor cursor, CXCursor, CXClientData client_data)
    {
        auto& ctx = *static_cast<VisitContext*>(client_data);
        auto& out = *ctx.out;

        switch (clang_getCursorKind (cursor))
        {
            case CXCursor_Constructor:
            {
                CXStringEx ctorName = clang_getCursorSpelling(cursor);
                CXStringEx ctorDisplayName = clang_getCursorDisplayName(cursor);

                int numArgs = clang_Cursor_getNumArguments(cursor);
                std::string paramTypes = "";
                std::string argNames = "";

                for (unsigned i = 0; i < numArgs; ++i) {
                    if (i != 0) {
                        paramTypes += ", ";
                        argNames += ", ";
                    }

                    auto paramCursor = clang_Cursor_getArgument (cursor, i);
                    auto paramType = clang_getCursorType(paramCursor);
                    auto paramName = toDSpelling(paramCursor);
                    if (paramName.empty())
                    {
                        paramName = "arg";
                        paramName += std::to_string(i);
                    }

                    if (ctx.forD)
                        paramTypes += toDSpelling(paramType);
                    else
                        paramTypes += clang_getCString(clang_getTypeSpelling(paramType));
                    if (paramTypes.back() != '*')
                      paramTypes += " ";
                    paramTypes += paramName;

                    argNames += paramName;
                }

                out << clang_getCString(ctorName);
                if (!ctx.forD)
                    out << "*";
                out << " new_" << clang_getCString(ctorName) << "(" << paramTypes << ")";
                if (ctx.forD)
                    out << " { return new " << clang_getCString(ctorName) << "(" << argNames << "); }";
                else
                    out << ";";
                out << "\n";

                break;
            }
            default:
                break;
        }
        return CXChildVisit_Continue;
    }

    CXChildVisitResult CursorVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
    {
        auto cursorFilename = cursorFilenameLocation (cursor);

        // Checks if this isn't an à-posteriori definition
        if ( !clang_equalCursors(clang_getCursorLexicalParent(cursor), clang_getCursorSemanticParent(cursor)) )
            return CXChildVisit_Continue;

        // Checks if this isn't just a preliminary declaration
        CXCursor cursorDefinition = clang_getCursorDefinition(cursor);
        if (!clang_Cursor_isNull(cursorDefinition) && !clang_isCursorDefinition(cursor)
                && clang_equalCursors(clang_getCursorLexicalParent(cursorDefinition), clang_getCursorSemanticParent(cursorDefinition))) // the last condition checks whether the definition occurs later but without leaving the same scope declaration
            return CXChildVisit_Continue;

        switch (clang_getCursorKind (cursor))
        {
//             case CXCursor_Namespace:
//                 clang_visitChildren (cursor, &CursorVisitor, client_data);
//                 break;
            case CXCursor_StructDecl:
            case CXCursor_ClassDecl:
                {
                    if (!isPolymorphic(cursor) || isAbstractClass(cursor))
                        break;

                    CXStringEx cursorSpelling = clang_getCursorSpelling(cursor);
                    if (strlen(clang_getCString(cursorSpelling)) == 0)
                      break;
                    if (clang_getCursorKind(parent) != CXCursor_TranslationUnit)
                        break;
                    clang_visitChildren(cursor, &CursorVisitor_Aggregate, client_data);
                }
                break;

            default:
                break;
        }

        return CXChildVisit_Continue;
    }
}

int main (int argc, const char **argv)
{
    if (argc < 2) {
        cerr << "Usage: ddmstructor_gen <.h files>\n";
        exit(1);
    }

    std::vector<const char*> cc1args({ "-x", "c++-header", "-std=c++11" });
    for(unsigned i = 1; i < argc; i++)
        cc1args.push_back(argv[i]);

    CXTranslationUnit transUnit;
    auto idx = clang_createIndex(0, 0);
    if (auto errCode = clang_parseTranslationUnit2(idx, nullptr, cc1args.data(), cc1args.size(),
                                            nullptr, 0, CXTranslationUnit_Incomplete, &transUnit))
        exit((int) errCode);

    for (unsigned i = 0, n = clang_getNumDiagnostics(transUnit); i != n; ++i) {
        auto diag = clang_getDiagnostic(transUnit, i);
        auto str = clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions());
        cerr << clang_getCString(str) << "\n";
        clang_disposeString(str);
    }

    std::ofstream outh;
    std::ofstream outd;
    outh.open("ddmdstructor.h");
    outd.open("ddmdstructor.d");

    outh << R"END(// Automatically generated by gen_ddmdstructor

#include "expression.h"

)END";

    VisitContext ctx;
    ctx.forD = false;
    ctx.out = &outh;
    unsigned result = clang_visitChildren(clang_getTranslationUnitCursor(transUnit),
                                           &CursorVisitor, &ctx);
    if (result)
        goto Lcleanup;

    outd << R"END(// Automatically generated by gen_ddmdstructor

import ddmd.root.port;
import ddmd.root.rootobject;
import ddmd.aggregate;
import ddmd.arraytypes;
import ddmd.cond;
import ddmd.complex;
import ddmd.dclass;
import ddmd.denum;
import ddmd.declaration;
import ddmd.dmodule;
import ddmd.dscope;
import ddmd.dstruct;
import ddmd.dsymbol;
import ddmd.expression;
import ddmd.func;
import ddmd.globals;
import ddmd.identifier;
import ddmd.dimport;
import ddmd.init;
import ddmd.mtype;
import ddmd.statement;
import ddmd.staticassert;
import ddmd.dtemplate;
import ddmd.tokens;
import ddmd.visitor;

import core.stdc.string;

extern(C++):

)END";

    ctx.forD = true;
    ctx.out = &outd;
    result = clang_visitChildren(clang_getTranslationUnitCursor(transUnit),
                                           &CursorVisitor, &ctx);

Lcleanup:
    clang_disposeTranslationUnit(transUnit);
    clang_disposeIndex(idx);

    outh.close();
    outd.close();

    return result;
}
