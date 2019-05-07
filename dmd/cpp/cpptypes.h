// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_TYPES_H
#define DMD_CPP_TYPES_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include <map>
#include <stack>
#include "cpp/calypso.h"
#include "cpp/cppmodule.h"
#include "mars.h"
#include "mtype.h"
#include "arraytypes.h"
#include "clang/AST/Type.h"
#include "clang/Basic/TargetInfo.h"

class Module;
class Dsymbol;
class Identifier;
class Import;
struct Scope;
class Type;
class TypeQualified;
class TypeFunction;

namespace clang
{
class Decl;
class ClassTemplateSpecializationDecl;
class TemplateParameterList;
}

namespace cpp
{
class Import;
class TypeQualifiedBuilder;

class BuiltinTypes
{
public:
    std::map<const clang::BuiltinType *, Type*> toD;
    std::map<Type*, const clang::BuiltinType *> toClang;

    void build(clang::ASTContext &Context);

protected:
    inline void map(clang::CanQualType &CQT, Type* t);
    Type *toInt(clang::TargetInfo::IntType intTy);
};

// Tweaks for special cases
enum TypeQualifiedBuilderOpts
{
    TQ_None = 0,
    TQ_OverOpSkipSpecArg = 1 << 0, // e.g skip "-" in opBinary!"-"
    TQ_OverOpFullIdent = 1 << 1, // prefer the non-templated function over the forwarding template
    TQ_PreferCachedSym = 1 << 2 // FIXME: temporary flag
};

class TypeMapper
{
public:
    TypeMapper(cpp::Module *mod = nullptr, bool isGlobal = false);  // mod can be null if no implicit import is needed
    virtual ~TypeMapper();

    bool addImplicitDecls = true;
    bool desugar = true;

    Scope* scSemImplicitImports = nullptr; // if non-null AddImplicitImportForDecl will semantic the imports (e.g for DeclReferencer)

    unsigned volatileNumber = 0; // number of volatile qualifiers found, needs to be reset when mapping functions

    // Clang -> DMD
    Type *fromType(const clang::QualType T, Loc loc);
    template<bool wantTuple = false>
     Objects *fromTemplateArguments(Loc loc, const clang::TemplateArgumentList *List,
                const clang::TemplateParameterList *ParamList = nullptr);

    class FromType // type-specific state
    {
    public:
        TypeMapper &tm;
        Loc loc;
        TypeQualified *prefix; // special case for NNS qualified types

        bool isDependent = false; // if dependent type do not assign TemplateInstance.Inst (causes issues with template default arguments)
        const clang::Expr *TypeOfExpr = nullptr;

        FromType(TypeMapper &tm, Loc loc, TypeQualified *prefix = nullptr);

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

    cpp::Import *AddImplicitImportForDecl(Loc loc, const clang::NamedDecl *D, bool fake = false);

protected:
    cpp::Module *mod;
    bool isGlobal;

    // Returns D->dsym or create it
    Dsymbol* dsymForDecl(Loc loc, const clang::NamedDecl* D);

    struct ImplicitImport
    {
        cpp::Import *im = nullptr;
        bool added = false;
    };
    llvm::SmallDenseMap<Module::RootKey, ImplicitImport, 4> implicitImports;

    Identifier *getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmDecl *D,
                                const clang::TemplateTypeParmType *T = nullptr);
    Identifier *getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D);

    const clang::Decl *GetRootForTypeQualified(clang::NamedDecl* D);

    cpp::Import *BuildImplicitImport(Loc loc, const clang::Decl *D, Identifier *aliasid = nullptr);
    cpp::Import *BuildImplicitImport(Loc loc, const clang::Decl *D, const clang::Module *Mod, Identifier *aliasid = nullptr);
    Module::RootKey GetImplicitImportKeyForDecl(const clang::NamedDecl *D);

    friend class cpp::TypeQualifiedBuilder;
};

RootObject *typeQualifiedRoot(TypeQualified *tqual);

const clang::RedeclarableTemplateDecl *getDefinition(const clang::RedeclarableTemplateDecl *D);
const clang::ClassTemplateSpecializationDecl *getDefinition(const clang::ClassTemplateSpecializationDecl *D);
const clang::VarTemplateSpecializationDecl *getDefinition(const clang::VarTemplateSpecializationDecl *D);

bool isNonSupportedType(clang::QualType T);

const clang::DeclContext *getDeclContextNonLinkSpec(const clang::Decl *D);
const clang::DeclContext *getDeclContextNamedOrTU(const clang::Decl *D); // skipping transparent (i.e anonymous) decl contexts too
const clang::Decl *GetNonNestedContext(const clang::Decl *D);  // returns the "root" for qualified types

const clang::Decl *getSpecializedDeclOrExplicit(const clang::Decl *Spec);
clang::QualType withoutNonAliasSugar(clang::QualType Ty);
const clang::TagDecl *isAnonTagTypedef(const clang::TypedefNameDecl* D);
bool isSameNameTagTypedef(const clang::TypedefNameDecl* D);

}

#endif
