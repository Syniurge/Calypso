// Contributed by Elie Morisse, same license DMD uses

#ifndef DMD_CPP_CPPDECLARATION_H
#define DMD_CPP_CPPDECLARATION_H

#ifdef __DMC__
#pragma once
#endif /* __DMC__ */

#include "root/root.h"
#include "arraytypes.h"
#include "dsymbol.h"
#include "enum.h"
#include "cpp/calypso.h"
#include "cpp/cppexpression.h"
#include "cpp/cpptypes.h"

#include "../declaration.h"

#include "clang/AST/RecursiveASTVisitor.h"

namespace clang
{
class ValueDecl;
class FunctionProtoType;
class FunctionDecl;
class CXXConstructorDecl;
class CXXDestructorDecl;
}

namespace cpp
{

class VarDeclaration : public ::VarDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::ValueDecl *VD;
    unsigned offsetInBits; // for bit fields
    bool isUsed = false;

    VarDeclaration(Loc loc, Identifier *id,
                   const clang::ValueDecl *VD, Type *t, Initializer *init = nullptr);
    VarDeclaration(const VarDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool isOverlappedWith(::VarDeclaration *v2) override;
};

class FuncDeclaration : public ::FuncDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::FunctionDecl *FD;
    bool isUsed = false;

    FuncDeclaration(Loc loc, Identifier *id, StorageClass storage_class,
                    Type* type, const clang::FunctionDecl *FD);
    FuncDeclaration(const FuncDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool functionSemantic3() override { return true; }
    bool allowFinalOverride() override { return true; }
    bool preferNonTemplateOverloads() override { return false; }

    static void doSemantic3(::FuncDeclaration *fd);
    static ::FuncDeclaration *overloadCppMatch(::FuncDeclaration *fd, const clang::FunctionDecl* FD);

    void accept(Visitor *v) override;
};

class CtorDeclaration : public ::CtorDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXConstructorDecl *CCD;
    bool isUsed = false;

    CtorDeclaration(Loc loc, StorageClass storage_class,
                    Type* type, const clang::CXXConstructorDecl *CCD);
    CtorDeclaration(const CtorDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool functionSemantic3() override { return true; }
    bool preferNonTemplateOverloads() override { return false; }

    void accept(Visitor *v) override;
};

class DtorDeclaration : public ::DtorDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::CXXDestructorDecl *CDD;
    bool isUsed = false;

    DtorDeclaration(Loc loc, StorageClass storage_class,
                    Identifier *id, const clang::CXXDestructorDecl *CDD);
    DtorDeclaration(const DtorDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    bool functionSemantic3() override { return true; }
    bool allowFinalOverride() override { return true; }
    bool preferNonTemplateOverloads() override { return false; }

    void accept(Visitor *v) override;
};

class EnumDeclaration : public ::EnumDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::EnumDecl *ED;

    EnumDeclaration(Loc loc, Identifier *id, Type *memtype,
            const clang::EnumDecl *ED);
    EnumDeclaration(const EnumDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = IgnoreNone) override;
    void complete() override;

    Expression *getDefaultValue(const Loc &loc) override;

    void accept(Visitor *v) override;
};

class EnumMember : public ::EnumMember
{
public:
    CALYPSO_LANGPLUGIN

    const clang::EnumConstantDecl *ECD;

    EnumMember(Loc loc, Identifier *id, Expression *value, Type *type,
               const clang::EnumConstantDecl *ECD);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void addMember(Scope *sc, ScopeDsymbol *sds) override;

    void accept(Visitor *v) override;
};

class AliasDeclaration : public ::AliasDeclaration
{
public:
    CALYPSO_LANGPLUGIN

    const clang::TypedefNameDecl *TND;
    bool isUsed = false;

    AliasDeclaration(Loc loc, Identifier *ident, Type *type,
            const clang::TypedefNameDecl *TND);
    AliasDeclaration(const AliasDeclaration&);
    Dsymbol *syntaxCopy(Dsymbol *s) override;
    void doSemantic();
    Dsymbol *toAlias() override;
    Dsymbol *toAlias2() override;

    void accept(Visitor *v) override;
};

const clang::FunctionDecl *getFD(::FuncDeclaration *f);

#define IMPLEMENT_syntaxCopy(Class, D) \
    Dsymbol* Class::syntaxCopy(Dsymbol* s) \
    { \
        if (!s) \
            s = new cpp::Class(*this); \
        assert(isCPP(s) && s->is##Class()); \
        static_cast<cpp::Class*>(s)->D = D; \
        return ::Class::syntaxCopy(s); \
    }
// NOTE: we use copy constructors only to copy the arguments passed to the main constructor, the rest is handled by syntaxCopy

// *************** //

bool isMapped(const clang::Decl *D);

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

    Dsymbol* VisitMacro(const clang::IdentifierInfo* II, const clang::Expr* E);

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

    Dsymbol* dsymForDecl(const clang::Decl* D);
    Dsymbol* dsymForDecl(const clang::NamedDecl* D,);

    Module* getModule(const clang::Decl* rootDecl);
    Package* getPackage(const clang::Decl* rootDecl);

    enum
    {
        MapTemplatePatterns = 1 << 0, // If not set pattern declarations describing templates will be discarded by VisitDecl (currently only VarDecl)
        MapTemplateInstantiations = 1 << 1,
        MapExplicitSpecs = 1 << 2, // If not set explicit and partial specs will be discarded by VisitDecl
        NamedValueWithAnonRecord = 1 << 3, // Only set when called from VisitValueDecl for e.g union {...} myUnion
        MapAnonRecord = 1 << 4,
        CreateTemplateInstance = 1 << 5,
    };


    static Identifier *getIdentifierForTemplateNonTypeParm(const clang::NonTypeTemplateParmDecl *NTTPD);
};

// *************** //

class NestedDeclMapper;

// Run semantic() on referenced functions and record decls to instantiate templates and have them codegen'd
class DeclReferencer : public clang::RecursiveASTVisitor<DeclReferencer>
{
    friend class NestedDeclMapper;

    DeclMapper mapper;
    ExprMapper expmap;

    Loc loc;
    Scope *sc = nullptr;

    bool Reference(const clang::NamedDecl *D);
    bool Reference(const clang::Type *T);

    bool VisitDeclRef(const clang::NamedDecl *D);
public:
    DeclReferencer(::Module* minst);

    void Traverse(Loc loc, Scope *sc, clang::Stmt *S);

    bool VisitCXXConstructExpr(const clang::CXXConstructExpr *E);
    bool VisitCXXNewExpr(const clang::CXXNewExpr *E);
    bool VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E);
    bool VisitDeclRefExpr(const clang::DeclRefExpr *E);
    bool VisitMemberExpr(const clang::MemberExpr *E);

    bool shouldVisitImplicitCode() const { return true; }
};

// Map records nested within a function, which includes lambda types.
class NestedDeclMapper : public clang::RecursiveASTVisitor<NestedDeclMapper>
{
    DeclReferencer& dref;

public:
    NestedDeclMapper(DeclReferencer& dref) : dref(dref) {}

    bool VisitLambdaExpr(const clang::LambdaExpr *E);
    bool TraverseCXXRecordDecl(const clang::CXXRecordDecl *D);

    bool shouldVisitImplicitCode() const { return true; }
};

Dsymbol* dsymForDecl(ScopeDsymbol* sds, const clang::Decl* D);

const clang::Decl *getCanonicalDecl(const clang::Decl *D); // the only difference with D->getCanonicalDecl() is that if the canonical decl is an out-of-ilne friend' decl and the actual decl is declared, this returns the latter instead of the former

Prot::Kind toProt(clang::AccessSpecifier AS);
bool isPolymorphic(const clang::RecordDecl *D);
void InstantiateFunctionDefinition(clang::Sema &S, clang::FunctionDecl* D);
void MarkFunctionReferenced(::FuncDeclaration* fd);
bool isTemplateInstantiation(const clang::Decl *D);

inline bool& getIsUsed(::FuncDeclaration* fd)
{
    assert(isCPP(fd));

    if (fd->isCtorDeclaration())
        return static_cast<cpp::CtorDeclaration*>(fd)->isUsed;
    else if (fd->isDtorDeclaration())
        return static_cast<cpp::DtorDeclaration*>(fd)->isUsed;
    else
        return static_cast<cpp::FuncDeclaration*>(fd)->isUsed;
}

}

#endif /* DMD_CPP_CPPDECLARATION_H */
