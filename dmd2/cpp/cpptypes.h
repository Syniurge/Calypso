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
    TQ_OverOpSkipSpecArg = 1 << 0, // skip "-" in opBinary!"-"
    TQ_OverOpFullIdent = 1 << 1 // prefer the non-templated function to the forwarding template
};

class TypeMapper
{
public:
    TypeMapper(cpp::Module *mod = nullptr);  // mod can be null if no implicit import is needed

    bool addImplicitDecls = true;
    Dsymbols *substsyms = nullptr; // only for TempateInstance::correctTiargs (partial spec arg deduction)
    bool desugar = false;
    bool cppPrefix = true; // if false, the ".cpp" prefix won't be appended, disabled only for « C++ global scope search »

    std::stack<const clang::Decl *> CXXScope;
    void rebuildScope(const clang::Decl *RightMost); // rebuild both CXXScope and TempParamScope

    // Clang -> DMD
    Type *fromType(const clang::QualType T, Loc loc);
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
        Type *fromTypeTemplateSpecialization(const clang::TemplateSpecializationType *T);
        Type *fromTypeTemplateTypeParm(const clang::TemplateTypeParmType *T, const clang::TemplateTypeParmDecl *OrigDecl = nullptr);
        Type *fromTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType *T);
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
        Type *typeSubstOrQualifiedFor(clang::NamedDecl* D,
                            const clang::TemplateArgument* ArgBegin = nullptr,
                            const clang::TemplateArgument* ArgEnd = nullptr);

        template<typename _Type>
         Type *fromTypeOfExpr(const _Type *T);

        const clang::TemplateTypeParmDecl *getOriginalTempTypeParmDecl(const clang::TemplateTypeParmType *T);

    private:
        Type *fromType(const clang::QualType T);  // private alias

        TypeQualified *fromNestedNameSpecifierImpl(const clang::NestedNameSpecifier *NNS);
    };

    // DMD -> Clang
    clang::QualType toType(Loc loc, Type* t, Scope *sc, StorageClass stc = STCundefined);
    
    ::Import *AddImplicitImportForDecl(Loc loc, const clang::NamedDecl *D, bool fake = false);

protected:
    cpp::Module *mod;

    llvm::SmallDenseMap<Module::RootKey, Import*, 8> implicitImports;
    llvm::DenseMap<const clang::NamedDecl*, Dsymbol*> declMap;  // fast lookup of mirror decls

    llvm::SmallVector<const clang::TemplateParameterList*, 4> TempParamScope;
    void pushTempParamList(const clang::Decl *D);
    Identifier *getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmDecl *D);
    Identifier *getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D);

    bool isInjectedClassName(const clang::Decl *D); // misleading name? not InjectedClassNameType
    bool isInjectedScopeName(const clang::Decl *D); // true if this is one of the CXXScope decls
    bool isRecursivelyInstantiated(const clang::TemplateName Name,
                    const clang::TemplateArgument *ArgBegin,
                    const clang::TemplateArgument *ArgEnd);

    const clang::Decl *GetRootForTypeQualified(clang::NamedDecl* D);

    ::Import *BuildImplicitImport(Loc loc, const clang::Decl *D, Identifier *aliasid = nullptr);
    ::Import *BuildImplicitImport(Loc loc, const clang::Decl *D, const clang::Module *Mod, Identifier *aliasid = nullptr);
    Module::RootKey GetImplicitImportKeyForDecl(const clang::NamedDecl *D);

    Type *trySubstitute(const clang::Decl *D);

    friend class cpp::TypeQualifiedBuilder;
};

RootObject *typeQualifiedRoot(TypeQualified *tqual);

const clang::ClassTemplateDecl *getDefinition(const clang::ClassTemplateDecl *D);
const clang::FunctionTemplateDecl *getDefinition(const clang::FunctionTemplateDecl *D);
const clang::ClassTemplateSpecializationDecl *getDefinition(const clang::ClassTemplateSpecializationDecl *D);

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
