// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPMODULE_H
#define DMD_CPP_CPPMODULE_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root/root.h"
#include "arraytypes.h"
#include "dsymbol.h"
#include "module.h"
#include "cpp/calypso.h"

#include <map>
#include <unordered_set>

namespace clang
{
class Decl;
}

namespace cpp {

class DeclMapper;

class Module : public ::Module
{
public:
    CALYPSO_LANGPLUGIN

    const clang::Decl *rootDecl;

    TemplateInstances tempinsts; // not added to members, but listed here to get emitted

    static std::map<const clang::Decl *, cpp::Module*> allCppModules;
    static Modules amodules_cpp; // redundant with allCppModules, but used by codegenModules

    // List of C++ symbols emitted in the existing object file
    // If a C++ symbol not in this list was referenced, the module needs to be re-gen'd
    std::unordered_set<std::string> emittedSymbols;
    bool needGen = false;

    static Package *rootPackage;    // package to store all C++ packages/modules, avoids name clashes (e.g std)
    static void init();

    Module(const char* filename, Identifier* ident);

    static Module *load(Loc loc, Identifiers *packages, Identifier *id, bool& isTypedef);

    void importAll(Scope *sc) override {}
    Dsymbol *search(const Loc& loc, Identifier *ident, int flags = IgnoreNone) override;
    void complete() override;

    void addPreambule() override;
    const char *manglePrefix() override { return "_Cpp"; }
    bool isCodegen() override { return true; } // §cpp modules aren't root, but they are codegen'd so non-instantiated functions need to be emitted

    void loadEmittedSymbolList();
    void saveEmittedSymbolList();

protected:
    bool searchedInlineNamespaces = false; // NOTE: they may need to get refreshed after a new cppmap
    std::vector<const clang::NamespaceDecl*> inlineNamespaces;

    struct NonMemberOverloadedOperators
    {
        std::vector<const clang::Decl*> OOs;
        bool searched = false;
    };
    std::map<clang::OverloadedOperatorKind, NonMemberOverloadedOperators> nonMemberOverloadedOperators;

    void searchInlineNamespaces();
    void searchNonMemberOverloadedOperators(clang::OverloadedOperatorKind Op); // map non-member overloaded operators and out-of-line friend decls
};

// *************** //

bool isMapped(const clang::Decl *D);

// Tweaks for special TypeQualifiedBuilder cases
enum TypeQualifiedBuilderOpts
{
    TQ_None = 0,
    TQ_OverOpSkipSpecArg = 1 << 0, // e.g skip "-" in opBinary!"-"
    TQ_OverOpFullIdent = 1 << 1, // prefer the non-templated function over the forwarding template
};

class DeclMapper
{
public:
    ::Module *minst, *importedFrom;
    DeclMapper(::Module *minst, ::Module *importedFrom) : minst(minst), importedFrom(importedFrom) {}
    DeclMapper(Dsymbol* s) : DeclMapper(s->getInstantiatingModule(), s->getModule()->importedFrom) {}

    /*
     * Declarations
     */
    Dsymbols *VisitDecl(const clang::Decl *D, unsigned flags = 0);

    Dsymbols *VisitValueDecl(const clang::ValueDecl *D, unsigned flags = 0);
    Dsymbols *VisitRecordDecl(const clang::RecordDecl* D, unsigned flags = 0);
    Dsymbols *VisitTypedefNameDecl(const clang::TypedefNameDecl *D);
    Dsymbols *VisitFunctionDecl(const clang::FunctionDecl *D, unsigned flags = 0);
    Dsymbols *VisitRedeclarableTemplateDecl(const clang::RedeclarableTemplateDecl* D);
    Dsymbols *VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D);
    Dsymbols *VisitVarTemplateSpecializationDecl(const clang::VarTemplateSpecializationDecl *D);
    Dsymbols *VisitEnumDecl(const clang::EnumDecl *D);
    Dsymbols *VisitEnumConstantDecl(const clang::EnumConstantDecl *D);

    TemplateParameter *VisitTemplateParameter(const clang::NamedDecl *Param,
                                              const clang::TemplateArgument *SpecArg = nullptr); // in DMD explicit specializations use parameters, whereas Clang uses args

    Dsymbol* dsymForMacro(Identifier* ident);

    template<typename PartialTy, typename SpecTy>
    Dsymbols *VisitTemplateSpecializationDecl(const SpecTy* D);

    template<typename SpecTy>
    Dsymbols* CreateTemplateInstanceFor(const SpecTy* D, Dsymbols* decldefs);

    // Entry points when mapping instances during semantic()
//     Dsymbol *VisitInstancedClassTemplate(const clang::ClassTemplateSpecializationDecl *D);
//     ::FuncDeclaration *VisitInstancedFunctionTemplate(const clang::FunctionDecl *D);
//     ::VarDeclaration *VisitInstancedVarTemplate(const clang::VarTemplateSpecializationDecl *D);

    /*
     * Types
     */
    static unsigned volatileNumber; // number of volatile qualifiers found, needs to be reset when mapping functions (LAZY does it?)

    // Clang -> DMD
    Type *fromType(const clang::QualType T, Loc loc);
    template<bool wantTuple = false>
    Objects *fromTemplateArguments(Loc loc, const clang::TemplateArgumentList *List,
                const clang::TemplateParameterList *ParamList = nullptr);

    class FromType // type-specific state
    {
    public:
        DeclMapper &mapper;
        Loc loc;
        TypeQualified *prefix; // special case for NNS qualified types

        const clang::Expr *TypeOfExpr = nullptr;

        FromType(DeclMapper &mapper, Loc loc, TypeQualified *prefix = nullptr);

        Type *operator()(const clang::QualType T);
        Type *fromTypeUnqual(const clang::Type *T);
        Type *fromTypeBuiltin(const clang::BuiltinType *T);
        Type *fromTypeComplex(const clang::ComplexType *T);
        Type *fromTypeArray(const clang::ArrayType *T);
        Type *fromTypeVector(const clang::VectorType *T);
        Type *fromTypeTypedef(const clang::TypedefType *T);
        Type *fromTypeEnum(const clang::EnumType *T);
        Type *fromTypeRecord(const clang::RecordType *T);
        Type *fromTypeMemberPointer(const clang::MemberPointerType *T);
        Type *fromTypeElaborated(const clang::ElaboratedType *T);
        Type *fromTypeUnaryTransform(const clang::UnaryTransformType *T);
        Type *fromTypeTemplateSpecialization(const clang::TemplateSpecializationType *T);
        Type *fromTypeTemplateTypeParm(const clang::TemplateTypeParmType *T);
        Type *fromTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType *T);
        Type *fromTypeSubstTemplateTypeParmPack(const clang::SubstTemplateTypeParmPackType* T);
        Type *fromTypeInjectedClassName(const clang::InjectedClassNameType *T);
        Type *fromTypeDependentName(const clang::DependentNameType *T);
        Type *fromTypeDependentTemplateSpecialization(const clang::DependentTemplateSpecializationType *T);
        Type *fromTypeTypeOfExpr(const clang::TypeOfExprType *T);
        Type *fromTypeDecltype(const clang::DecltypeType *T);
        Type *fromTypePackExpansion(const clang::PackExpansionType *T);
        TypeFunction *fromTypeFunction(const clang::FunctionProtoType *T,
                        const clang::FunctionDecl *FD = nullptr);

        template<bool wantTuple = false>
            Objects *fromTemplateArgument(const clang::TemplateArgument *Arg,
                                          const clang::NamedDecl *Param = nullptr);  // NOTE: Param is required when the parameter type is an enum, because in the AST enum template arguments are resolved to uint while DMD expects an enum constant or it won't find the template decl. Is this a choice or a compiler bug/limitation btw?
        template<bool wantTuple = false>
            Objects *fromTemplateArguments(const clang::TemplateArgument *First,
                                           const clang::TemplateArgument *End,
                                           const clang::TemplateParameterList *ParamList = nullptr);
        TypeQualified *fromNestedNameSpecifier(const clang::NestedNameSpecifier *NNS);
        TypeQualified *fromTemplateName(const clang::TemplateName Name,
                    const clang::TemplateArgument *ArgBegin = nullptr,
                    const clang::TemplateArgument *ArgEnd = nullptr);  // returns a template or a template instance
                // if it's a template it's not actually a type but a symbol, but that's how parsing TemplateAliasParameter works anyway

        TypeQualified *typeQualifiedFor(clang::NamedDecl* D,
                                        const clang::TemplateArgument* ArgBegin = nullptr,
                                        const clang::TemplateArgument* ArgEnd = nullptr,
                                        TypeQualifiedBuilderOpts options = TQ_None);

        template<typename _Type>
        Type *fromTypeOfExpr(const _Type *T);

    private:
        Type *fromType(const clang::QualType T);  // private alias

        TypeQualified *fromNestedNameSpecifierImpl(const clang::NestedNameSpecifier *NNS);
    };

    // DMD -> Clang
    clang::QualType toType(Loc loc, Type* t, Scope *sc, StorageClass stc = STCundefined);

    Identifier *getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmDecl *D,
                                                 const clang::TemplateTypeParmType *T = nullptr);
    Identifier *getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D);

    //

    enum Flags
    {
        NoFlag = 0,
        MapTemplatePatterns = 1 << 0, // If not set pattern declarations describing templates will be discarded by VisitDecl (currently only VarDecl)
//         MapTemplateInstantiations = 1 << 1,
        MapExplicitSpecs = 1 << 2, // If not set explicit and partial specs will be discarded by VisitDecl
//         NamedValueWithAnonRecord = 1 << 3, // Only set when called from VisitValueDecl for e.g union {...} myUnion
        MapAnonRecord = 1 << 4,
        CreateTemplateInstance = 1 << 5,
    };

    template<Flags flags = NoFlag> Dsymbol* dsymForDecl(const clang::Decl* D);
    template<Flags flags = NoFlag> Dsymbol* dsymForDecl(const clang::NamedDecl* D);

    Module* getModule(const clang::Decl* D);
    Package* getPackage(const clang::Decl* D);


    static Identifier *getIdentifierForTemplateNonTypeParm(const clang::NonTypeTemplateParmDecl *NTTPD);
};

typedef DeclMapper::Flags DeclMapperFlags;

template<DeclMapperFlags flags = DeclMapper::NoFlag>
Dsymbol* dsymForDecl(ScopeDsymbol* sds, const clang::Decl* D);

const clang::Decl *getCanonicalDecl(const clang::Decl *D); // the only difference with D->getCanonicalDecl() is that if the canonical decl is an out-of-line friend' decl and the actual decl is declared, this returns the latter instead of the former

Prot::Kind toProt(clang::AccessSpecifier AS);
bool isPolymorphic(const clang::RecordDecl *D);
void InstantiateFunctionDefinition(clang::Sema &S, clang::FunctionDecl* D);
void MarkFunctionReferenced(::FuncDeclaration* fd);
bool isTemplateInstantiation(const clang::Decl *D);
bool isTemplateParameterPack(const clang::NamedDecl *Param);

}

#endif /* DMD_CPP_CPPMODULE_H */
