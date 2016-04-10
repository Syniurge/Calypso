// Contributed by Elie Morisse, same license DMD uses

#include "cpp/modulemap.h"
#include "aggregate.h"
#include "attrib.h"
#include "declaration.h"
#include "enum.h"
#include "identifier.h"
#include "import.h"
#include "init.h"
#include "identifier.h"
#include "template.h"
#include "scope.h"
#include "statement.h"
#include "id.h"
#include "driver/cl_options.h"

#include "cpp/calypso.h"
#include "cpp/cppmodule.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppimport.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppexpression.h"
#include "cpp/cpptemplate.h"

#include <stdlib.h>
#include <string>

#include "llvm/ADT/DenseMap.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Lookup.h"

extern llvm::cl::opt<bool> preservePaths;

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

/********************************/

Package *Module::rootPackage;
Modules Module::amodules;

void Module::init()
{
    rootPackage = new Package(Identifier::idPool("cpp"));
    rootPackage->symtab = new DsymbolTable;

    modules->insert(rootPackage);
}

static void combine(char *&objfn, Identifier *id)
{
    auto prevobjfn = objfn;

    auto objlen = strlen(objfn);
    auto idlen = id->len;
    objfn = (char *)mem.xmalloc(objlen + 1 + idlen + 1);
    memcpy(objfn, prevobjfn, objlen);
    objfn[objlen] = '_';
    objlen++;
    memcpy(objfn + objlen, id->string, idlen + 1);

    mem.xfree(prevobjfn);
}

Module::Module(const char* filename, Identifier* ident, Identifiers *packages)
    : ::Module(nullptr, ident, 0, 0)
{
    srcfile = new File(filename);

    // Let's not create directories for the time being
    const char *objPrefix = "__cpp";
    char *objfn = strdup(objPrefix);

    // e.g __cpp_package_package_module.o
    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];
        combine(objfn, pid);
    }
    combine(objfn, ident);

    arg = objfn;
}

Dsymbol *Module::search(Loc loc, Identifier *ident, int flags)
{
    auto result = ::Module::search(loc, ident, flags);

    if ((flags & IgnorePrivateMembers) && result && result->isImport())
        return nullptr; // semi-HACK? this makes the imports inside an imported module invisible to D code,
                                // but also prevents conflicts caused by the choice to name C++ modules after record names.

    return result;
}

void Module::addPreambule()
{
    // Statically import object.d for object and size_t (used by buildXtoHash)

    // TODO This still makes "object" susceptible to collide with C++ names.
    // We could eventually choose a random unused alias name if necessary.
    if (members->dim == 0 || ((*members)[0])->ident != Id::object)
    {
        ::Import *im = new ::Import(Loc(), NULL, Id::object, NULL, true);
        members->shift(im);
    }
}

File *Module::buildFilePath(const char *, const char *path, const char *ext)
{
    const char *argobj;
    if (preservePaths)
        argobj = this->arg;
    else
        argobj = FileName::name(this->arg);

    if (!opts::cppCacheDir.empty())
        path = opts::cppCacheDir.c_str();

    assert(!FileName::absolute(argobj));
    argobj = FileName::combine(path, argobj);

    FileName::ensurePathExists(FileName::path(argobj));

    // always append the extension! otherwise hard to make output switches consistent
    return new File(FileName::forceExt(argobj, ext));
}

/************************************/

// DeclMapper::DeclMapper(Module* mod)
//     : TypeMapper(mod)
// {
// }

inline Prot DeclMapper::toProt(clang::AccessSpecifier AS)
{
    switch(AS) {
        case clang::AS_public:
            return PROTpublic;
        case clang::AS_protected:
            return PROTprotected;
        case clang::AS_private:
            return PROTprivate;
        case clang::AS_none:
            return PROTnone;
        default:
            return PROTundefined;
    }
}

/*****/

namespace
{

inline Dsymbols *oneSymbol(Dsymbol *s)
{
    auto decldefs = new Dsymbols;
    decldefs->push(s);
    return decldefs;
}

}

Dsymbols *DeclMapper::VisitDeclContext(const clang::DeclContext *DC)
{
    auto decldefs = new Dsymbols;

    for (auto D = DC->decls_begin(), DEnd = DC->decls_end();
        D != DEnd; ++D)
    {
        if (auto d = VisitDecl(*D))
            decldefs->append(d);
    }

    return decldefs;
}

Dsymbols *DeclMapper::VisitDecl(const clang::Decl *D, unsigned flags)
{
    if (D != getCanonicalDecl(D))
        return nullptr;

    // Sometimes the canonical decl of an explicit spec isn't the one in the parent DeclContext->decls
    // but the decl in FunctionTemplateDecl->specs, ex.: __convert_to_v in locale_facets.h
    // Which is why we map each spec in VisitRedeclarableTemplateDecl and need a flag to ensure that
    // they get mapped only once.
    if (!(flags & MapExplicitSpecs))
    {
        if (isa<clang::ClassTemplateSpecializationDecl>(D))
            return nullptr;

        if (auto FD = dyn_cast<clang::FunctionDecl>(D))
            if (FD->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization &&
                        FD->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization)
                return nullptr;
    }

    Dsymbols *s = nullptr;

#define DECL(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D);
#define DECLWF(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D, flags);

    if (0) ;
    DECL(TypedefName)
    DECL(ClassTemplateSpecialization)
    DECLWF(Record)
    DECLWF(Function)
    DECL(RedeclarableTemplate)
    DECL(Enum)
    DECL(Value)

#undef DECL
#undef DECLWF

    return s;
}

Dsymbols *DeclMapper::VisitValueDecl(const clang::ValueDecl *D)
{
    auto& Context = calypso.getASTContext();
    ExprMapper expmap(*this);

    if (isNonSupportedType(D->getType()))
        return nullptr;

    if (isa<clang::IndirectFieldDecl>(D)) // implicit fields injected from anon unions/structs, which are already mapped
        return nullptr;

    if (auto Field = dyn_cast<clang::FieldDecl>(D))
    {
        if (Field->isUnnamedBitfield())
            return nullptr;

        // NOTE: in union {...} myUnion isAnonymousStructOrUnion() will be false, it returns true only for "true" anonymous structs/unions
        if (Field->isAnonymousStructOrUnion())
        {
            auto a = VisitDecl(Field->getType()->castAs<clang::RecordType>()->getDecl(), MapAnonRecord);
            assert(a->dim == 1 && (*a)[0]->isAttribDeclaration());

            auto anon = static_cast<AnonDeclaration*>((*a)[0]->isAttribDeclaration());
            anon->AnonField = Field;
            return a;
        }
    }

    auto loc = fromLoc(D->getLocation());
    auto decldefs = new Dsymbols;

    auto id = fromIdentifier(D->getIdentifier());
    Type *t = nullptr;

    auto Ty = withoutNonAliasSugar(D->getType());
    if (auto RT = dyn_cast<clang::RecordType>(Ty.getTypePtr()))
    {
        auto RD = RT->getDecl();
        if (!RD->getIdentifier())
        {
            // Special case of union {...} myUnion; without D equivalment
            auto r = VisitRecordDecl(RD, NamedValueWithAnonRecord);
            if (!r)
                return nullptr;

            assert(r->dim == 1 && r->data[0]->isAggregateDeclaration());
            auto ad = static_cast<AggregateDeclaration*>(r->data[0]);

            t = ad->type;
            decldefs->push(ad);
        }
    }

    if (!t)
        t = fromType(D->getType(), loc);

    if (!t)
        return nullptr;

    if (t->isConst())
        t = t->immutableOf();

    auto a = new VarDeclaration(loc, id, D, t);

    if (auto Var = dyn_cast<clang::VarDecl>(D))
    {
        if (Var->hasExternalStorage())
            a->storage_class |= STCextern;

        if (Var->getTLSKind() == clang::VarDecl::TLS_Dynamic)
            a->storage_class |= STCtls;

        if (Var->isStaticDataMember())
            a->storage_class |= STCstatic;

        if ((Var->isConstexpr() || t->isImmutable()) &&
                Var->getAnyInitializer())
        {
            // we avoid initializer expressions except for const/constexpr variables
            auto Init = Var->getAnyInitializer();
            clang::APValue Eval;
            llvm::SmallVector<clang::PartialDiagnosticAt, 2> Diags;

            Expression *e = nullptr;
            if (!Init->isValueDependent() && Init->EvaluateAsInitializer(Eval, Context, Var, Diags))
                e = expmap.fromAPValue(loc, Eval, Var->getType()); // evaluating integer and boolean expressions is always preferable, because in some rare cases
                    // DMD and Clang's intepretations differ, one important instance being -1u < 0u (true for DMD, false for Clang)

            if (!e)
                e = expmap.fromExpression(Var->getAnyInitializer(), true);

            if (e && e->op != TOKnull)
                if (Init->isInstantiationDependent() || Init->isEvaluatable(Context))
                    a->init = new ExpInitializer(loc, e);
        }
    }

    decldefs->push(a);
    return decldefs;
}

static void MarkFunctionForEmit(const clang::FunctionDecl *D)
{
    auto& S = calypso.getSema();
    auto& Diags = calypso.getDiagnostics();

    if (!D->getDeclContext()->isDependentContext())
    {
        auto D_ = const_cast<clang::FunctionDecl*>(D);
        
        auto FPT = dyn_cast<clang::FunctionProtoType>(D_->getType());
        if (FPT && clang::isUnresolvedExceptionSpec(FPT->getExceptionSpecType()))
            S.ResolveExceptionSpec(D->getLocation(), FPT);

        S.MarkFunctionReferenced(D->getLocation(), D_);
        if (Diags.hasErrorOccurred())
        {
            assert(D->isInvalidDecl());
            Diags.Reset();
        }
        S.PerformPendingInstantiations();

        // MarkFunctionReferenced won't instantiate some implicitly instantiable functions
        // Not fully understanding why, but here's a second attempt
        if (!D->hasBody() && D->isImplicitlyInstantiable())
            S.InstantiateFunctionDefinition(D->getLocation(), D_);
    }
}

// For simplicity's sake (or confusion's) let's call records with either virtual functions or bases polymorphic
inline bool isPolymorphic(const clang::RecordDecl *D)
{
    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();
    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
        return CRD && D->isCompleteDefinition() &&
            (CRD->getNumBases() || CRD->isPolymorphic());
}

Dsymbols *DeclMapper::VisitRecordDecl(const clang::RecordDecl *D, unsigned flags)
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.getSema();
    auto& Diags = *calypso.pch.Diags;
    auto Canon = D->getCanonicalDecl();

    if (D->isImplicit() && !(flags & MapImplicitRecords))
        return nullptr;

    auto decldefs = new Dsymbols;
    auto loc = fromLoc(D->getLocation());

    if (S.RequireCompleteType(D->getLocation(), Context.getRecordType(D), 0))
        Diags.Reset();

    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();
    bool isDefined = D->isCompleteDefinition();
    bool isStruct = !isPolymorphic(D) && !(flags & ForcePolymorphic);

    auto TND = D->getTypedefNameForAnonDecl();

    int anon = 0;
    if (D->isAnonymousStructOrUnion())
    {
        assert(!TND);

        if (!(flags & MapAnonRecord))
          return nullptr;

        anon = 1;
        if (D->isUnion())
            anon = 2;
    }

    if (!anon && !getIdentifierOrNull(D) &&
                !(flags & NamedValueWithAnonRecord))
        return nullptr; // special case for union {} myUnion; which has no D equivalent

    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
        // NOTE: CXXRecordDecl will disappear in a future version of Clang and only
        // RecordDecl will remain to be used for both C and C++.

    AggregateDeclaration *a;
    if (!anon)
    {
        auto id = getIdentifierOrNull(D);

        if (D->isUnion())
        {
            a = new UnionDeclaration(loc, id, D);
        }
        else if (isStruct)
        {
            a = new StructDeclaration(loc, id, D);
        }
        else
        {
            auto baseclasses = new BaseClasses;

            if (CRD)
            {
                for (auto B = CRD->bases_begin(),
                        BEnd = CRD->bases_end(); B != BEnd; ++B)
                {
                    auto brt = fromType(B->getType(), loc);

                    baseclasses->push(new BaseClass(brt,
                                                    toProt(B->getAccessSpecifier())));
                }
            }

            auto cd = new ClassDeclaration(loc, id, baseclasses, CRD);
            a = cd;
        }

        declMap[D] = a;
    }

    CXXScope.push(D);

    // atm we're sortof mirroring parseAggregate()
    auto members = new Dsymbols;

    if (!isDefined)
        goto Ldeclaration;

    if (CRD && !D->isUnion())
    {
//         if (isStruct && !CRD->hasDefaultConstructor())
//         {
//             // Disable the default ctor if it's also disabled on the C++ side
//             auto tf = new TypeFunction(new Parameters, nullptr, 0, LINKd, STCdisable);
//             auto disabledCtor = new CtorDeclaration(loc, STCdisable, tf, nullptr);
//             members->push(disabledCtor);
//         }

        if (!CRD->isDependentType())
        {
            auto _CRD = const_cast<clang::CXXRecordDecl *>(CRD);

            auto MarkEmit = [&] (clang::FunctionDecl *FD) {
                if (FD) MarkFunctionForEmit(FD);
            };

            // Clang declares and defines implicit ctors/assignment operators lazily,
            // but they need to be emitted all in the record module.
            // Mark them for emit here since they won't be visited.
            MarkEmit(S.LookupDefaultConstructor(_CRD));
            for (int i = 0; i < 2; i++)
                MarkEmit(S.LookupCopyingConstructor(_CRD, i ? clang::Qualifiers::Const : 0));
            
            MarkEmit(S.LookupDestructor(_CRD));

            for (int i = 0; i < 2; i++)
                for (int j = 0; j < 2; j++)
                    for (int k = 0; k < 2; k++)
                        MarkEmit(S.LookupCopyingAssignment(_CRD, i ? clang::Qualifiers::Const : 0, j ? true : false,
                                                  k ? clang::Qualifiers::Const : 0));
        }
    }

    if (CRD && D->isInvalidDecl())
    {
        // Despite being invalid themselves methods of invalid records are not always marked invalid for some reason.
        // We need to mark everyone invalid, because if we don't emitting debug info for them will trigger an assert.
        for (auto MD: CRD->methods())
            if (!MD->isStatic())
                MD->setInvalidDecl();
    }

    // Add specific decls: fields, vars, tags, templates, typedefs
    // They are expected by DMD to be in the correct order.
    for (auto M: D->decls())
    {
        if (cast<clang::Decl>(M->getDeclContext())->getCanonicalDecl() != Canon)
            continue;  /* only map declarations that are semantically within the RecordDecl */

        if (!isa<clang::FieldDecl>(M) && !isa<clang::VarDecl>(M) &&
              !isa<clang::FunctionDecl>(M) && !isa<clang::TagDecl>(M) &&
              !isa<clang::RedeclarableTemplateDecl>(M) && !isa<clang::TypedefNameDecl>(M))
            continue;

        if (auto s = VisitDecl(M))
            members->append(s);
    }

Ldeclaration:
    CXXScope.pop();

    if (anon)
        decldefs->push(new AnonDeclaration(loc, anon == 2, members));
    else
    {
        a->members = members;
        decldefs->push(a);
    }

    // Sometimes friend declarations are the only existing declarations, so map them to the parent context
    // see friend QString::operator==(const QString &s1, const QString &s2);
    // NOTE: should be after because ClassDeclaration::semantic() expects decldefs[0] to be the record
    typedef clang::DeclContext::specific_decl_iterator<clang::FriendDecl> Friend_iterator;
    auto CTSD = dyn_cast<clang::ClassTemplateSpecializationDecl>(D);
    if (!CTSD || CTSD->getSpecializationKind() != clang::TSK_ImplicitInstantiation) // if we're in an implicit instantiation, no need to remap the out-of-line specializations which have their own template mapped with the class template
    {
        for (Friend_iterator I(D->decls_begin()), E(D->decls_end());
                    I != E; I++)
        {
            auto Decl = (*I)->getFriendDecl();
            if (!Decl || !Decl->isOutOfLine())
                continue;

            auto Func = dyn_cast<clang::FunctionDecl>(Decl);
            if (!Func || Func->isDependentContext()) // HACK FIXME: map them as template decls using the tpl from the record
                continue;

            if (auto s = VisitDecl(Decl))
                decldefs->append(s);
        }
    }

    return decldefs;
}

Dsymbols *DeclMapper::VisitTypedefNameDecl(const clang::TypedefNameDecl* D)
{
    auto& Context = calypso.getASTContext();
    auto Ty = D->getUnderlyingType();

    if (isAnonTagTypedef(D))
        return nullptr;  // the anon tag will be mapped by VisitRecordDecl to an aggregate named after the typedef identifier

    if (isSameNameTagTypedef(D)) // e.g typedef union pthread_attr_t pthread_attr_t needs to be discarded
        return nullptr;

    if (D->isImplicit())
        if (Ty == Context.Int128Ty || Ty == Context.UnsignedInt128Ty)
            return nullptr; // any function using those typedefs will be discarded anyway

    auto loc = fromLoc(D->getLocation());
    auto id = fromIdentifier(D->getIdentifier());
    auto t = fromType(D->getUnderlyingType(), loc);

    if (!t)
        return nullptr;

    auto a = new AliasDeclaration(loc, id, t, D);
    return oneSymbol(a);
}

TemplateParameters *initTempParams(Loc loc, SpecValue &spec)
{
    auto tpl = new TemplateParameters;
    TemplateParameter *p = nullptr;

    if (spec.op)
    {
        auto dstringty = new TypeIdentifier(loc, Id::object);
        dstringty->addIdent(Identifier::idPool("string"));

        auto tp_specvalue = new StringExp(loc, const_cast<char*>(spec.op));
        p = new TemplateValueParameter(loc, Identifier::idPool("op"),
                                            dstringty, tp_specvalue, nullptr);
    }
    else if (spec.t)
        p = new TemplateTypeParameter(loc, Identifier::idPool("type"), spec.t, nullptr);

    if (p)
        tpl->push(p);
    return tpl;
}

namespace {
class FunctionReferencer : public clang::RecursiveASTVisitor<FunctionReferencer>
{
    DeclMapper &mapper;
    clang::Sema &S;
    clang::SourceLocation SLoc;

    Loc loc;
    llvm::DenseSet<const clang::FunctionDecl *> Referenced;

    bool Reference(const clang::FunctionDecl *Callee);
    bool ReferenceRecord(const clang::RecordType *RT);
public:
    FunctionReferencer(DeclMapper &mapper,
                        clang::Sema &S, clang::SourceLocation SLoc)
        : mapper(mapper), S(S), SLoc(SLoc), loc(fromLoc(SLoc)) {}
    bool VisitCallExpr(const clang::CallExpr *E);
    bool VisitCXXConstructExpr(const clang::CXXConstructExpr *E);
    bool VisitCXXNewExpr(const clang::CXXNewExpr *E);
    bool VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E);
};

bool FunctionReferencer::Reference(const clang::FunctionDecl *D)
{
    auto Callee = const_cast<clang::FunctionDecl*>(D);
    if (!Callee || Callee->isDeleted() ||
            Callee->getBuiltinID() || Referenced.count(Callee->getCanonicalDecl()))
        return true;
    if (Callee->isInvalidDecl())
        return false;
    Referenced.insert(Callee->getCanonicalDecl());

    MarkFunctionForEmit(Callee);

    if (Callee->isInvalidDecl())
        return false;

    mapper.AddImplicitImportForDecl(loc, Callee);

    const clang::FunctionDecl *Def;
    if (!Callee->hasBody(Def))
        return true;

    TraverseStmt(Def->getBody());
    return true;
}

bool FunctionReferencer::ReferenceRecord(const clang::RecordType *RT)
{
    auto RD = cast<clang::CXXRecordDecl>(RT->getDecl());
    if (!RD->hasDefinition())
        return true;

    mapper.AddImplicitImportForDecl(loc, RD);

    if (!RD->isDependentType())
    {
        if (isPolymorphic(RD))
        {
            auto Ctor = S.LookupDefaultConstructor(
                            const_cast<clang::CXXRecordDecl *>(RD));
            Reference(Ctor);
        }
        Reference(RD->getDestructor());
    }
    return true;
}

bool FunctionReferencer::VisitCallExpr(const clang::CallExpr *E)
{
    return Reference(E->getDirectCallee());
}

bool FunctionReferencer::VisitCXXConstructExpr(const clang::CXXConstructExpr *E)
{
    auto ConstructedType = E->getType();
    if (!ConstructedType.isNull()) {
        if (const clang::RecordType *RT = ConstructedType->getAs<clang::RecordType>())
            ReferenceRecord(RT);
    }

    return Reference(E->getConstructor());
}

bool FunctionReferencer::VisitCXXNewExpr(const clang::CXXNewExpr *E)
{
    return Reference(E->getOperatorNew());
}

bool FunctionReferencer::VisitCXXDeleteExpr(const clang::CXXDeleteExpr *E)
{
    auto DestroyedType = E->getDestroyedType();
    if (!DestroyedType.isNull()) {
        if (const clang::RecordType *RT = DestroyedType->getAs<clang::RecordType>())
            ReferenceRecord(RT);
    }

    return Reference(E->getOperatorDelete());
}
}

bool isMapped(const clang::Decl *D) // TODO
{
    if (auto FD = dyn_cast<clang::FunctionDecl>(D))
    {
        if (D->isInvalidDecl())
            return false;

        if (isa<clang::FunctionNoProtoType>(FD->getType()))
            return false; // functions without prototypes are afaik builtins, and since D needs a prototype they can't be mapped

        if (auto MD = dyn_cast<clang::CXXMethodDecl>(D))
        {
            auto Parent = MD->getParent();
            if (Parent->isUnion())
                return false;
            if (MD->isImplicit() && MD->isTrivial()/* && !isPolymorphic(Parent)*/)
                return false;
        }
    }

    return true;
}

Dsymbols *DeclMapper::VisitFunctionDecl(const clang::FunctionDecl *D, unsigned flags)
{
    auto& S = calypso.getSema();

    if (!isMapped(D))
        return nullptr;

    if (!(flags & MapTemplateInstantiations) && D->isTemplateInstantiation() &&
            D->getTemplatedKind() != clang::FunctionDecl::TK_MemberSpecialization)
        return nullptr;

    auto loc = fromLoc(D->getLocation());
    auto MD = dyn_cast<clang::CXXMethodDecl>(D);

    MarkFunctionForEmit(D);
    if (D->isInvalidDecl())
        return nullptr;

    const clang::FunctionDecl *Def;
    if (D->hasBody(Def))
        FunctionReferencer(*this, S, clang::SourceLocation()).TraverseStmt(Def->getBody());

    auto FPT = D->getType()->castAs<clang::FunctionProtoType>();

    auto tf = FromType(*this, loc).fromTypeFunction(FPT, D);
    if (!tf)
    {
        ::warning(loc, "Discarding %s, non-supported argument or return type (e.g int128_t)",
                            D->getDeclName().getAsString().c_str());
        return nullptr;
    }
    assert(tf->ty == Tfunction);

    StorageClass stc = STCundefined;
    if (MD)
    {
        if (MD->isDeleted())
            stc |= STCdisable;

        if (MD->isStatic())
            stc |= STCstatic;

        if (!MD->isVirtual())
            stc |= STCfinal;

        if (MD->isPure())
            stc |= STCabstract;

        if (MD->begin_overridden_methods()
                != MD->end_overridden_methods())
            stc |= STCoverride;

        if (isa<clang::CXXDestructorDecl>(D))
            stc &= ~(STCoverride|STCabstract); // dtors aren't added to D's VTBLs
    }
    tf->addSTC(stc);
    
    auto a = new Dsymbols;
    ::FuncDeclaration *fd;
    if (auto CD = dyn_cast<clang::CXXConstructorDecl>(D))
    {
        fd = new CtorDeclaration(loc, stc, tf, CD);
    }
    else if (auto DD = dyn_cast<clang::CXXDestructorDecl>(D))
    {
        fd = new DtorDeclaration(loc, stc, Id::dtor, DD);
    }
    else if (D->isOverloadedOperator() || isa<clang::CXXConversionDecl>(D))
    {
        SpecValue spec(*this);
        auto opIdent = getIdentifierOrNull(D, &spec); // will return nullptr if the operator isn't supported by D
                            // TODO map the unsupported operators anyway

        if (!opIdent)
            return nullptr;

        // NOTE: C++ overloaded operators might be virtual, unlike D which are always final (being templates)
        //   Mapping the C++ operator to opBinary()() directly would make D lose info and overriding the C++ method impossible

        auto R = D->getDeclContext()->lookup(D->getDeclName());
        std::function<bool(const clang::NamedDecl*)> pred;
        SpecValue spec2(*this);
        if (isa<clang::TagDecl>(D->getDeclContext()))
            pred = [&](const clang::NamedDecl* _D)
                { return opIdent == getIdentifierOrNull(_D, &spec2); }; // member operator, simplest case
        else
        {
            // non member overloaded operators are trickier, since they end up in different modules and we need one alias per module
            auto OpTyDecl = isOverloadedOperatorWithTagOperand(D);
            if (!OpTyDecl)
                pred = [&](const clang::NamedDecl* _D)
                    { return opIdent == getIdentifierOrNull(_D, &spec2) && !isOverloadedOperatorWithTagOperand(_D); };
            else
                pred = [&](const clang::NamedDecl* _D)
                    { return opIdent == getIdentifierOrNull(_D, &spec2) && isOverloadedOperatorWithTagOperand(_D, OpTyDecl); };
        }

        auto FirstOverload = *std::find_if(R.begin(), R.end(), pred);
        assert(FirstOverload);
        bool isFirstOverloadInScope = FirstOverload->getCanonicalDecl() == D->getCanonicalDecl();

        bool wrapInTemp = spec &&
                    !D->getDescribedFunctionTemplate() &&  // if it's a templated overloaded operator then the template declaration is already taken care of
                    !(D->isFunctionTemplateSpecialization() && D->isTemplateInstantiation());  // if we're instantiating a templated overloaded operator, we're after the function

        Identifier *fullIdent;
        if (wrapInTemp)
            fullIdent = getExtendedIdentifier(D, *this);
        else
            fullIdent = opIdent;

        // Add the overridable method (or the static function)
        fd = new FuncDeclaration(loc, fullIdent, stc, tf, D);
        a->push(fd);

        if (wrapInTemp && isFirstOverloadInScope)
        {
            // Add the opUnary/opBinary/... template declaration aliasing fullIdent if none exists(important!)
            auto tpl = initTempParams(loc, spec);

            auto a_fwd = new ::AliasDeclaration(loc, opIdent,
                                        new TypeIdentifier(loc, fullIdent));

            // Enclose the forwarding function within the template declaration
            auto decldefs = new Dsymbols;
            decldefs->push(a_fwd);

            auto tempdecl = new ::TemplateDeclaration(loc, opIdent, tpl, nullptr, decldefs);
            a->push(tempdecl);
        }

        return a;
    }
    else
    {
        auto id = fromIdentifier(D->getIdentifier());
        fd = new FuncDeclaration(loc, id, stc, tf, D);
    }

    if (D->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization &&
            D->getPrimaryTemplate()) // weird, but the explicit instantiation of basic_istream<char>::getline is considered an explicit specialization
    {
        auto tpl = new TemplateParameters;

        auto FT = D->getPrimaryTemplate();
        auto TPL = FT->getTemplateParameters();
        auto AI = D->getTemplateSpecializationArgs()->asArray().begin();

        TempParamScope.push_back(TPL);
        for (auto PI = TPL->begin(), PE = TPL->end();
            PI != PE; PI++)
        {
            auto tp = VisitTemplateParameter(*PI, AI);
            if (!tp)
                return nullptr;
            tpl->push(tp);

            if (AI) AI++;
        }
        TempParamScope.pop_back();

        auto decldefs = new Dsymbols;
        decldefs->push(fd);
        auto td = new TemplateDeclaration(loc, fd->ident, tpl, decldefs, D);
        return oneSymbol(td);
    }

    a->push(fd);
    return a;
}

bool isTemplateParameterPack(const clang::NamedDecl *Param)
{
    if (auto NTTPD = dyn_cast<clang::NonTypeTemplateParmDecl>(Param))
        return NTTPD->isParameterPack();
    else if (auto TTPD = dyn_cast<clang::TemplateTypeParmDecl>(Param))
        return TTPD->isParameterPack();
    else if (auto TempTemp = dyn_cast<clang::TemplateTemplateParmDecl>(Param))
        return TempTemp->isParameterPack();

    llvm::llvm_unreachable_internal();
}

Dsymbols *DeclMapper::VisitRedeclarableTemplateDecl(const clang::RedeclarableTemplateDecl *D)
{
    if (!isa<clang::ClassTemplateDecl>(D) && !isa<clang::TypeAliasTemplateDecl>(D)
         && !isa<clang::FunctionTemplateDecl>(D))
        return nullptr; // temporary

    SpecValue spec(*this);

    auto loc = fromLoc(D->getLocation());
    auto id = getIdentifierOrNull(D, &spec);

    if (!id)
        return nullptr; // TODO: map unsupported overloaded operators

    auto CTD = dyn_cast<clang::ClassTemplateDecl>(D);
    auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D);

    auto Def = D;
    if (CTD)
        Def = getDefinition(CTD);
    else if (FTD)
        Def = getDefinition(FTD);

    auto tpl = initTempParams(loc, spec);
    auto TPL = Def->getTemplateParameters();

    TempParamScope.push_back(TPL);

    // FIXME: C++ templates may have multiple parameter packs, which isn't supported by D
    // Since this is a rare occurence ignore them for now
    bool packFound = false;

    for (auto P: *TPL)
    {
        if (packFound)
        {
            ::warning(loc, "Template %s has more than one parameter pack, ignore for now", id->toChars());
            return nullptr;
        }
        packFound = isTemplateParameterPack(P);

        auto tp = VisitTemplateParameter(P);
        if (!tp)
            return nullptr; // should be extremely rare, e.g if there's a int128_t value parameter
        tpl->push(tp);
    }

    auto s = VisitDecl(getCanonicalDecl(Def->getTemplatedDecl()));

    if (!s)
        return nullptr;

    auto decldefs = new Dsymbols;
    decldefs->append(s);

    TempParamScope.pop_back();

    auto td = new TemplateDeclaration(loc, id, tpl, decldefs, D);

    auto a = new Dsymbols;
    a->push(td);

    // Append explicit and partial specializations to the returned symbols as well
    if (CTD)
    {
        llvm::SmallVector<clang::ClassTemplatePartialSpecializationDecl *, 2> PS;
        const_cast<clang::ClassTemplateDecl*>(CTD)->getPartialSpecializations(PS);

        for (auto Spec: CTD->specializations())
            if (auto sp = VisitDecl(Spec->getCanonicalDecl(), MapExplicitSpecs))
                a->append(sp);

        for (auto PartialSpec: PS)
            if (auto sp = VisitDecl(PartialSpec->getCanonicalDecl(), MapExplicitSpecs))
                a->append(sp);
    }
    else if (FTD)
    {
        for (auto Spec: FTD->specializations())
            if (auto sp = VisitDecl(getCanonicalDecl(Spec), MapExplicitSpecs))
                a->append(sp);
    }

    return a;
}

Identifier *DeclMapper::getIdentifierForTemplateNonTypeParm(const clang::NonTypeTemplateParmDecl *T)
{
    if (auto Id = T->getIdentifier())
        return fromIdentifier(Id);
    else
    {
        ::warning(Loc(), "Generating identifier for anonymous C++ non-type template parameter");

        // This should only ever happen in template param decl mapping
        std::string str;
        llvm::raw_string_ostream OS(str);
        OS << "value_parameter_" << T->getDepth() << '_' << T->getIndex();

        return Identifier::idPool(OS.str().c_str());
    }
}

TemplateParameter *DeclMapper::VisitTemplateParameter(const clang::NamedDecl *Param,
                                                                      const clang::TemplateArgument *SpecArg)
{
    ExprMapper expmap(*this);
    TemplateParameter *tp;

    auto loc = fromLoc(Param->getLocation());
    Identifier *id;

    if (auto NTTPD =
            dyn_cast<clang::NonTypeTemplateParmDecl>(Param))
    {
        id = getIdentifierForTemplateNonTypeParm(NTTPD);
        auto valTy = fromType(NTTPD->getType(), loc);

        if (!valTy || isNonSupportedType(NTTPD->getType()))
            return nullptr;

        if (NTTPD->isParameterPack())
        {
            ::warning(Loc(), "%s", "C++ template non-type parameter packs do not strictly map to D tuple parameters");
            return new TemplateTupleParameter(loc, id);
        }
        else
        {
            Expression *tp_specvalue = nullptr;
            Expression *tp_defaultvalue = nullptr;

            if (SpecArg)
            {
                switch (SpecArg->getKind())
                {
                    case clang::TemplateArgument::Expression:
                        tp_specvalue = expmap.fromExpression(SpecArg->getAsExpr());
                        break;
                    case clang::TemplateArgument::Integral:
                        tp_specvalue = expmap.fromAPInt(loc, SpecArg->getAsIntegral());
                        break;
                    case clang::TemplateArgument::NullPtr:
                        tp_specvalue = new NullExp(Loc()/*, fromType(SpecArg->getNullPtrType())*/);
                        break;
                    default:
                        assert(false && "Unsupported template specialization value");
                }

                tp_specvalue = isExpression(FromType(*this, loc).fromTemplateArgument(SpecArg));
                assert(tp_specvalue);
            }

            if (NTTPD->hasDefaultArgument())
            {
//                 tp_defaultvalue = expmap.fromExpression(NTTPD->getDefaultArgument());

                // TEMPORARY HACK: we choose a simple default value to make defaultArg()'s life easier
                //  (there were obscure identifier errors, e.g in __iterator_traits<_normal_iterator>).
                //  This doesn't affect anything other than reflection since the default argument evaluation is done by Clang
                //  We could btw use Clang's evaluation while keeping the mapped expression for reflection
                tp_defaultvalue = new DotIdExp(loc, new TypeExp(loc, valTy), Id::init);
            }

            tp = new TemplateValueParameter(loc, id, valTy,
                                        tp_specvalue, tp_defaultvalue);
        }

    }
    else if (auto TTPD =
            dyn_cast<clang::TemplateTypeParmDecl>(Param))
    {
        id = getIdentifierForTemplateTypeParm(TTPD);

        if (TTPD->isParameterPack())
        {
            ::warning(Loc(), "%s", "C++ template type parameter packs do not strictly map to D tuple parameters");
            return new TemplateTupleParameter(loc, id);
        }
        else
        {
            Type *tp_spectype = nullptr;
            Type *tp_defaulttype = nullptr;

            if (SpecArg)
            {
                auto SpecTy = SpecArg->getAsType();
                auto specArg = FromType(*this, loc).fromTemplateArgument(SpecArg);
                if (!specArg || isNonSupportedType(SpecTy))
                    return nullptr; // might be a non supported type
                tp_spectype = isType(specArg);
                assert(tp_spectype);
            }

            if (TTPD->hasDefaultArgument())
                tp_defaulttype = fromType(TTPD->getDefaultArgument(), loc);

            tp = new TemplateTypeParameter(loc, id, tp_spectype, tp_defaulttype);
        }
    }
    else if (auto TempTemp =
            dyn_cast<clang::TemplateTemplateParmDecl>(Param))
    {
        id = getIdentifierForTemplateTemplateParm(TempTemp);

        if (TempTemp->isParameterPack())
        {
            ::warning(Loc(), "%s", "C++ template template parameter packs do not strictly map to D tuple parameters");
            return new TemplateTupleParameter(loc, id);
        }
        else
        {
            Type *tp_spectype = nullptr;
            Type *tp_defaulttype = nullptr;

            if (SpecArg)
            {
                tp_spectype = isType(FromType(*this, loc).fromTemplateArgument(SpecArg));
                assert(tp_spectype);
            }

            if (TempTemp->hasDefaultArgument())
                tp_defaulttype = FromType(*this, loc).fromTemplateName(
                        TempTemp->getDefaultArgument().getArgument().getAsTemplate());

            tp = new TemplateAliasParameter(loc, id, nullptr, tp_spectype, tp_defaulttype);
        }
    }
    else assert(false && "unrecognized template parameter");

    return tp;
}

Dsymbol *DeclMapper::VisitInstancedClassTemplate(const clang::ClassTemplateSpecializationDecl *D,
                                                 unsigned flags)
{
    assert(!isa<clang::ClassTemplatePartialSpecializationDecl>(D));

    rebuildScope(cast<clang::Decl>(D->getDeclContext()));
    pushTempParamList(D);

    auto a = VisitRecordDecl(D, flags | MapExplicitSpecs | MapTemplateInstantiations);
    assert(a->dim);
    return (*a)[0];
}

::FuncDeclaration *DeclMapper::VisitInstancedFunctionTemplate(const clang::FunctionDecl *D)
{
    rebuildScope(cast<clang::Decl>(D->getDeclContext()));
    pushTempParamList(D);

    auto a = VisitDecl(D, MapExplicitSpecs | MapTemplateInstantiations);
    assert(a->dim == 1 && (*a)[0]->isFuncDeclaration() && isCPP((*a)[0]));
    return static_cast<::FuncDeclaration*>((*a)[0]);
}

// Explicit specializations only
Dsymbols *DeclMapper::VisitClassTemplateSpecializationDecl(const clang::ClassTemplateSpecializationDecl *D)
{   TemplateDeclaration *a;

    D = getDefinition(D);

    if (!D->isExplicitSpecialization())
        return nullptr;

    auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(D);
        // NOTE: D's partial specializations != C++'s partial specializations
        // The mapping provides a "close" but not exact approximation of equivalent template specs in D (for reflection),
        // but TemplateDeclaration::findBestMatch is skipped since the choice is done by Clang anyway.

    auto loc = fromLoc(D->getLocation());
    auto id = fromIdentifier(D->getIdentifier());

    auto tpl = new TemplateParameters;

    auto CT = getDefinition(D->getSpecializedTemplate());
    auto TPL = CT->getTemplateParameters();
    auto AI = D->getTemplateArgs().asArray().begin();

    if (Partial)
    {
        TPL = Partial->getTemplateParameters(); // SEMI-HACK #1 because we alter the tiargs to match the partial spec params
        AI = nullptr; // SEMI-HACK #2 because partial spec args won't matter during semantic
    }
    TempParamScope.push_back(TPL);

    for (auto PI = TPL->begin(), PE = TPL->end();
        PI != PE; PI++)
    {
        auto tp = VisitTemplateParameter(*PI, AI);
        if (!tp)
            return nullptr;
        tpl->push(tp);

        if (AI) AI++;
    }

    if (!Partial)
        TempParamScope.pop_back(); // the depth of template parameters does not consider explicit specs to be in the TempParamScope

    auto decldefs = new Dsymbols;
    auto ad = VisitRecordDecl(D);
    decldefs->append(ad);

    if (Partial)
        TempParamScope.pop_back();

    a = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    return oneSymbol(a);
}

Dsymbols *DeclMapper::VisitEnumDecl(const clang::EnumDecl* D)
{
    if (!D->isCompleteDefinition())
    {
        D = D->getDefinition();
        if (!D)
            return nullptr; // forward declaration without definition, skip
    }

    auto loc = fromLoc(D->getLocation());
    auto ident = getIdentifierOrNull(D);

    Type *memtype = nullptr;
    if (!D->isDependentType())
    {
        auto IntType = D->getIntegerType();
        if (IntType.isNull())
            IntType = D->getPromotionType();

        memtype = fromType(IntType, loc);
    }

    auto e = new EnumDeclaration(loc, ident, memtype, D);
    CXXScope.push(D);

    for (auto ECD: D->enumerators())
    {
        auto memberLoc = fromLoc(ECD->getLocation());

        if (!e->members)
            e->members = new Dsymbols;

        auto ident = fromIdentifier(ECD->getIdentifier());
        Expression *value = nullptr;

        if (auto InitE = ECD->getInitExpr())
        {
            value = ExprMapper(*this).fromExpression(InitE);
            value = new CastExp(memberLoc, value, memtype); // SEMI-HACK (?) because the type returned by 1LU << ... will be ulong and we may need an int (see wctype.h)
        }

        auto em = new EnumMember(memberLoc, ident, value, nullptr);
        e->members->push(em);
    }

    CXXScope.pop();
    return oneSymbol(e);
}

/*****/

std::string moduleName(Identifiers *packages, Identifier *ident)
{
    std::string result = "__cpp/";
    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];
        result.append(pid->string, pid->len);
        result.append("/");
    }
    result.append(ident->string, ident->len);
    return result;
}

// Look into namespace redecls if there are any
static clang::DeclContext::lookup_const_result lookup(const clang::DeclContext *DC,
                                                   Identifier *id)
{
    auto& Table = calypso.getPreprocessor().getIdentifierTable();

    const char prefix[] = u8"§";
    bool prefixed = strncmp(id->string, prefix, sizeof(prefix)-1) == 0;
    auto& II = Table.get(!prefixed ? id->string : id->string + sizeof(prefix)-1);

    return DC->lookup(clang::DeclarationName(&II));
}

const clang::TagDecl *isOverloadedOperatorWithTagOperand(const clang::Decl *D,
                                const clang::NamedDecl *SpecificTag)
{
    auto& Context = calypso.getASTContext();

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
        Func = FuncTemp->getTemplatedDecl();

    if (!Func || !Func->isOverloadedOperator())
        return nullptr;

    if (Func->isFunctionTemplateSpecialization())
        Func = Func->getPrimaryTemplate()->getTemplatedDecl();

    assert(Func->getNumParams() > 0);
    if (Func->getNumParams() > 2)
        return nullptr; // [] and () cannot be non-member (FIXME: not entirely sure about (), couldn't find the source)

    if (auto ClassTemp = llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(SpecificTag))
        SpecificTag = ClassTemp->getTemplatedDecl();

    const clang::TagDecl *OpTyDecl = nullptr;

    for (unsigned I = 0; I < Func->getNumParams(); I++)
    {
        auto ParamTy = Func->getParamDecl(I)->getType().getNonReferenceType()
                                .getDesugaredType(Context).getCanonicalType();

        if (auto TempSpec = ParamTy->getAs<clang::TemplateSpecializationType>())
            if (auto Temp = TempSpec->getTemplateName().getAsTemplateDecl())
            {
                if (auto ClassTemp = dyn_cast<clang::ClassTemplateDecl>(Temp))
                {
                    OpTyDecl = ClassTemp->getTemplatedDecl();
                    break;
                }

                if (isa<clang::TypeAliasTemplateDecl>(Temp))
                    assert(false && "Wrongly assumed that it would get desugared");
            }

        if (auto TagTy = ParamTy->getAs<clang::TagType>())
        {
            OpTyDecl = TagTy->getDecl();
            break;
        }
    }

    if (OpTyDecl)
    {
        OpTyDecl = cast<clang::TagDecl>(GetNonNestedContext(OpTyDecl)); // get the module-level TagDecl

        if (auto TempSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(OpTyDecl))
            OpTyDecl = TempSpec->getSpecializedTemplate()->getTemplatedDecl();
    }

    if (OpTyDecl && // either LHS or RHS has a tag type
          (!SpecificTag || OpTyDecl->getCanonicalDecl() == SpecificTag->getCanonicalDecl())) // if we're looking for a specific type, compare it
        return OpTyDecl; 

    return nullptr;
}

static clang::Module *tryFindClangModule(Loc loc, Identifiers *packages, Identifier *id,
                                         Package *&p, size_t i)
{
#ifdef USE_CLANG_MODULES
    auto MMap = calypso.pch.MMap;

    if (!MMap)
        return nullptr;

    Package *pkg = p;
    clang::Module *M = nullptr;

    for (; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];

        llvm::StringRef name(pid->string, pid->len);
        M = MMap->lookupModuleQualified(name, M);
        if (!M)
            return nullptr;

        pkg = static_cast<Package*>(pkg->symtab->lookup(pid));
        assert(pkg);
    }

    llvm::StringRef name(id->string, id->len);
    M = MMap->lookupModuleQualified(name, M);
    if (!M)
        return nullptr;

    p = pkg;
    return M;
#else
    return nullptr;
#endif
}

static inline bool isTopLevelInNamespaceModule (const clang::Decl *D)
{
    auto Tag = dyn_cast<clang::TagDecl>(D);
    if (Tag && (Tag->getIdentifier() || Tag->getTypedefNameForAnonDecl()))
        return false; // anonymous tags are added as well

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    if (Func && Func->getDescribedFunctionTemplate())
        return false; // the function template will get mapped instead

    if (!Tag && !Func && !isa<clang::VarDecl>(D) &&
            !isa<clang::TypedefNameDecl>(D) &&
            !isa<clang::FunctionTemplateDecl>(D) &&
            !isa<clang::TypeAliasTemplateDecl>(D))
        return false;

    if (isOverloadedOperatorWithTagOperand(D))
        return false;  // non-member overloaded operators with class/struct/enum operands are included in their own module

    return true;
}

static void mapNamespace(DeclMapper &mapper,
                             const clang::DeclContext *DC,
                             Dsymbols *members,
                             bool forClangModule = false)
{
    auto CanonDC = cast<clang::Decl>(DC)->getCanonicalDecl();
    auto MMap = calypso.pch.MMap;
    auto& SrcMgr = calypso.getSourceManager();

    auto D = DC->decls_begin(),
            DE = DC->decls_end();

    for (; D != DE; ++D)
    {
        if (cast<clang::Decl>(D->getDeclContext())->getCanonicalDecl()
                != CanonDC)
            continue;  // only map declarations that are semantically within the DeclContext

        auto DLoc = SrcMgr.getFileLoc((*D)->getLocation());
#ifdef USE_CLANG_MODULES
        if (!forClangModule && DLoc.isValid() && DLoc.isFileID()
                && MMap->findModuleForHeader(
                    SrcMgr.getFileEntryForID(SrcMgr.getFileID(DLoc))))
            continue;  // skip decls which are parts of a Clang module
#endif

        auto InnerNS = dyn_cast<clang::NamespaceDecl>(*D);
        if ((InnerNS && InnerNS->isInline()) || isa<clang::LinkageSpecDecl>(*D))
        {
            mapNamespace(mapper, cast<clang::DeclContext>(*D), members, forClangModule);
            continue;
        }
        else if (!isTopLevelInNamespaceModule(*D))
            continue;

        if (auto s = mapper.VisitDecl(*D))
            members->append(s);
    }
}

static void mapClangModule(DeclMapper &mapper,
                             const clang::Decl *Root,
                             clang::Module *M,
                             Dsymbols *members)
{
    auto AST = calypso.getASTUnit();
    auto& SrcMgr = calypso.getSourceManager();

    llvm::SmallVector<clang::Decl*, 32> RegionDecls;

    // HACK-ish but Clang doesn't offer a straightforward way
    // SourceManager::translateFile() only offers the first FID of a FileEntry which doesn't contain all the decls,
    // so we need to loop over all the FID corresponding to Header.Entry.
    for (unsigned I = 0, N = SrcMgr.loaded_sloc_entry_size(); I != N; ++I)
    {
        auto& SLoc = SrcMgr.getLoadedSLocEntry(I);
        if (SLoc.isFile() && SLoc.getFile().getContentCache())
        {
            for (auto Header: M->Headers[clang::Module::HK_Normal])
            {
                if (SLoc.getFile().getContentCache()->OrigEntry != Header.Entry)
                    continue;

                auto Loc = clang::SourceLocation::getFromRawEncoding(SLoc.getOffset());
                auto FID = SrcMgr.getFileID(Loc); // NOTE: getting a FileID without a SourceLocation is impossible, it's locked tight
                auto SID = SrcMgr.getLocForStartOfFile(FID).getRawEncoding();
                AST->findFileRegionDecls(FID, 0, (1U << 31) - 1 - SID, RegionDecls); // passed Length is the maximum value before offset overflow kicks in
            }
        }
    }

    // Not forgetting namespace redecls
    llvm::SmallVector<clang::Decl*, 8> RootDecls, ParentDecls;

    std::function<bool(const clang::Decl *)>
        Corresponding = [&] (const clang::Decl *D)
    {
        if (isa<clang::TranslationUnitDecl>(D))
            return true;

        if (!Corresponding(cast<clang::Decl>(D->getDeclContext())->getCanonicalDecl()))
            return false;

        ParentDecls.swap(RootDecls);
        RootDecls.clear();

        if (!ParentDecls.empty())
            for (auto Parent: ParentDecls)
            {
                auto DC = cast<clang::DeclContext>(Parent);
                for (auto ModDecl: DC->decls())
                    if (D->getCanonicalDecl() == ModDecl->getCanonicalDecl())
                        RootDecls.push_back(ModDecl);
            }
        else
            for (auto ModDecl: RegionDecls)
                if (isa<clang::TranslationUnitDecl>(D->getDeclContext()))
                    if (D->getCanonicalDecl() == ModDecl->getCanonicalDecl())
                        RootDecls.push_back(ModDecl);

        return !RootDecls.empty();
    };

    if (!Corresponding(Root))
    {
        ::error(Loc(), "Incorrect import, Clang module doesn't contain requested namespaces");
        fatal();
    }

    std::function<void(const clang::Decl *)> Map = [&] (const clang::Decl *D)
    {
        if (auto LinkSpec = dyn_cast<clang::LinkageSpecDecl>(D))
        {
            for (auto LD: LinkSpec->decls())
                Map(LD);
            return;
        }

        auto DC = D->getDeclContext();
        assert(isa<clang::TranslationUnitDecl>(DC)
                || isa<clang::NamespaceDecl>(DC)
                || isa<clang::LinkageSpecDecl>(DC));

        if (!isTopLevelInNamespaceModule(D))
            return;

        if (auto s = mapper.VisitDecl(D))
            members->append(s);
    };

    if (!isa<clang::TranslationUnitDecl>(Root))
        for (auto R: RootDecls)
            mapNamespace(mapper, cast<clang::DeclContext>(R), members, true);
    else
        for (auto D: RegionDecls)
            if (isa<clang::TranslationUnitDecl>(D->getDeclContext()))
                Map(D);
}

Module *Module::load(Loc loc, Identifiers *packages, Identifier *id)
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.getSema();
    auto& Diags = calypso.pch.Diags;

    S.CurContext = Context.getTranslationUnitDecl(); // HACK? Needed for declaring implicit ctors and dtors
    if (!S.TUScope)
        // Clang BUG? TUScope isn't set when no Parser is used, but required by template instantiations (e.g LazilyCreateBuiltin)
        S.TUScope = new clang::Scope(nullptr, clang::Scope::DeclScope, *Diags);

    const clang::DeclContext *DC = Context.getTranslationUnitDecl();
    Package *pkg = rootPackage;

    assert(packages && packages->dim);

    clang::Module *M = nullptr;
    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];

        auto R = lookup(DC, pid);
        if (R.empty())
        {
            // Check if there's a Clang module matching the remaining packages.module.
            // Note that if there is a Clang module named QtCore, import Qt.QtCore is correct and
            // will import the declarations inside Qt:: and inside the headers listed in QtCore.
            M = tryFindClangModule(loc, packages, id, pkg, i);
            if (M)
                break;

            ::error(loc, "no C++ package named %s", pid->toChars());
            fatal();
        }

        auto NSN = dyn_cast<clang::NamespaceDecl>(R[0]);
        if (!NSN || NSN->isInline())
        {
            ::error(loc, "only non-inline namespaces can be C++ packages");
            fatal();
        }

        pkg = static_cast<Package*>(pkg->symtab->lookup(pid));
        assert(pkg);

        DC = NSN;
    }

    if (!M)
        M = tryFindClangModule(loc, packages, id, pkg, packages->dim);

    auto m = new Module(moduleName(packages, id).c_str(),
                        id, packages);
    m->members = new Dsymbols;
    m->parent = pkg;
    m->loc = loc;

    DeclMapper mapper(m);

    if (M)
    {
        auto D = cast<clang::Decl>(DC)->getCanonicalDecl();
        m->rootKey.first = D;
        m->rootKey.second = M;
        mapClangModule(mapper, D, M, m->members);
    }
    else if (strcmp(id->string, "_") == 0)  // Hardcoded module with all the top-level non-tag decls + the anonymous tags of a namespace which aren't in a Clang module
    {
        m->rootKey.first = cast<clang::Decl>(DC)->getCanonicalDecl();

        auto NS = dyn_cast<clang::NamespaceDecl>(DC);
        if (!NS)
        {
            assert(isa<clang::TranslationUnitDecl>(DC));

            mapNamespace(mapper, DC, m->members);
        }
        else
        {
            auto I = NS->redecls_begin(),
                    E = NS->redecls_end();

            for (; I != E; ++I)
            {
                DC = *I;
                mapNamespace(mapper, DC, m->members);
            }
        }
    }
    else
    {
        clang::NamedDecl *D = nullptr;

        // Lookups can't find the implicit __va_list_tag record
        if (id == Identifier::idPool("__va_list_tag") && packages->dim == 1)
        {
            D = Context.getVaListTagType()
                ->getAs<clang::RecordType>()->getDecl();
        }
        else
        {
            auto R = lookup(DC, id);
            if (R.empty())
            {
                ::error(loc, "no C++ module named %s", id->toChars());
                fatal();
            }

            // Module must be a record or enum
            for (auto Match: R)
            {
                if (auto Typedef = dyn_cast<clang::TypedefNameDecl>(Match))
                    if (auto Tag = isAnonTagTypedef(Typedef))
                        Match = const_cast<clang::TagDecl*>(Tag);

                if (isa<clang::TagDecl>(Match) || isa<clang::ClassTemplateDecl>(Match))
                    D = Match;
            }

            if (!D)
            {
                ::error(loc, "C++ modules have to be records (class/struct, template or not) or enums");
                fatal();
            }
        }

        if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
            D = Spec->getSpecializedTemplate();

        D = cast<clang::NamedDecl>(D->getCanonicalDecl());
        auto CTD = dyn_cast<clang::ClassTemplateDecl>(D);

        if (CTD)
            m->rootKey.first = CTD->getTemplatedDecl();
        else
            m->rootKey.first = D;

        if (auto s = mapper.VisitDecl(D, DeclMapper::MapImplicitRecords))
            m->members->append(s);

        // Add the non-member overloaded operators that are meant to work with this record/enum
        for (int Op = 1; Op < clang::NUM_OVERLOADED_OPERATORS; Op++)
        {
            auto OpName = Context.DeclarationNames.getCXXOperatorName(
                        static_cast<clang::OverloadedOperatorKind>(Op));

            for (auto Ctx = D->getDeclContext(); Ctx; Ctx = Ctx->getLookupParent())
            {
                if (Ctx->isTransparentContext())
                    continue;

                for (auto OverOp: Ctx->lookup(OpName))
                    if (isOverloadedOperatorWithTagOperand(OverOp, D))
                        if (auto s = mapper.VisitDecl(getCanonicalDecl(OverOp)))
                            m->members->append(s);
            }
        }

//         srcFilename = AST->getSourceManager().getFilename(TD->getLocation());
    }
    
    amodules.push_back(m);
    pkg->symtab->insert(m);
    return m;
}

}
