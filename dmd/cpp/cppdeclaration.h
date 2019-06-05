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

bool isMapped(const clang::Decl *D);

class DeclMapper : public TypeMapper
{
public:
    Dsymbols importDecls, pendingTempinsts;

    DeclMapper(::Module *mod, bool isGlobal = false, bool addImplicitDecls = true) : TypeMapper(mod, isGlobal, addImplicitDecls) {}

    inline Prot::Kind toProt(clang::AccessSpecifier AS);

    // Declarations
    Dsymbols *VisitDeclContext(const clang::DeclContext *DC);

    Dsymbols *VisitDecl(const clang::Decl *D, unsigned flags = 0);

    Dsymbols *VisitValueDecl(const clang::ValueDecl *D, unsigned flags = 0);
    Dsymbols *VisitRecordDecl(const clang::RecordDecl* D, unsigned flags = 0);
    Dsymbols *VisitTypedefNameDecl(const clang::TypedefNameDecl *D);
    Dsymbols *VisitFunctionDecl(const clang::FunctionDecl *D, unsigned flags = 0);
    Dsymbols *VisitRedeclarableTemplateDecl(const clang::RedeclarableTemplateDecl* D);
    Dsymbols *VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D);
    Dsymbols *VisitVarTemplateSpecializationDecl(const clang::VarTemplateSpecializationDecl *D);
    Dsymbols *VisitEnumDecl(const clang::EnumDecl *D);

    // Entry points when mapping instances during semantic()
    Dsymbol *VisitInstancedClassTemplate(const clang::ClassTemplateSpecializationDecl *D);
    ::FuncDeclaration *VisitInstancedFunctionTemplate(const clang::FunctionDecl *D);
    ::VarDeclaration *VisitInstancedVarTemplate(const clang::VarTemplateSpecializationDecl *D);

    TemplateParameter *VisitTemplateParameter(const clang::NamedDecl *Param,
                                              const clang::TemplateArgument *SpecArg = nullptr); // in DMD explicit specializations use parameters, whereas Clang uses args

    Dsymbol* VisitMacro(const clang::IdentifierInfo* II, const clang::Expr* E);

    template<typename SpecTy>
    Dsymbols* CreateTemplateInstanceFor(Loc loc, const SpecTy* D, Dsymbols* decldefs);
    template<typename PartialTy, typename SpecTy>
    Dsymbols *VisitTemplateSpecializationDecl(const SpecTy* D);

    enum
    {
        ForcePolymorphic = 1 << 0, // When a templace declaration is polymorphic, we want the explicit template specializations to be polymorphic too even if isPolymorphic() is false
        MapTemplatePatterns = 1 << 1, // If not set pattern declarations describing templates will be discarded by VisitDecl (currently only VarDecl)
        MapTemplateInstantiations = 1 << 2,
        MapExplicitSpecs = 1 << 3, // If not set explicit and partial specs will be discarded by VisitDecl
        NamedValueWithAnonRecord = 1 << 4, // Only set when called from VisitValueDecl for e.g union {...} myUnion
        MapAnonRecord = 1 << 5,
        CreateTemplateInstance = 1 << 6,
    };


    static Identifier *getIdentifierForTemplateNonTypeParm(const clang::NonTypeTemplateParmDecl *NTTPD);
};

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

const clang::Decl *getCanonicalDecl(const clang::Decl *D); // the only difference with D->getCanonicalDecl() is that if the canonical decl is an out-of-ilne friend' decl and the actual decl is declared, this returns the latter instead of the former
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
