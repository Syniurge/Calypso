// Contributed by Elie Morisse, same license DMD uses

#include "cpp/astunit.h"
#include "aggregate.h"
#include "attrib.h"
#include "declaration.h"
#include "enum.h"
#include "identifier.h"
#include "import.h"
#include "init.h"
#include "lexer.h"
#include "template.h"
#include "scope.h"
#include "statement.h"
#include "id.h"

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
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Sema.h"

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
    rootPackage = new Package(Lexer::idPool("cpp"));
    rootPackage->symtab = new DsymbolTable;

    modules->insert(rootPackage);
}

static void combine(char *&objfn, Identifier *id)
{
    auto prevobjfn = objfn;

    auto objlen = strlen(objfn);
    auto idlen = id->len;
    objfn = (char *)mem.malloc(objlen + 1 + idlen + 1);
    memcpy(objfn, prevobjfn, objlen);
    objfn[objlen] = '_';
    objlen++;
    memcpy(objfn + objlen, id->string, idlen + 1);

    mem.free(prevobjfn);
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

/************************************/

// DeclMapper::DeclMapper(Module* mod)
//     : TypeMapper(mod)
// {
// }

inline PROT DeclMapper::toPROT(clang::AccessSpecifier AS)
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

Dsymbols *DeclMapper::VisitDecl(const clang::Decl *D)
{
    if (!D->isCanonicalDecl())
        return nullptr;

    Dsymbols *s = nullptr;

    // Unfortunately a long ugly list of if (... dyn_cast...) is more solid and
    // future-proof than a pretty switch à la decl_visitor

#define DECL(BASE) \
    else if (const clang::BASE##Decl *BASE##D = \
                            dyn_cast<clang::BASE##Decl>(D)) \
        s = Visit##BASE##Decl(BASE##D);

    if (0) ;
    DECL(TypedefName)
    DECL(ClassTemplateSpecialization)
    DECL(Record)
    DECL(Function)
    DECL(RedeclarableTemplate)
    DECL(Enum)
    DECL(Value)

#undef DECL

    return s;
}

Dsymbols *DeclMapper::VisitValueDecl(const clang::ValueDecl *D)
{
    auto& Context = calypso.getASTContext();
    ExprMapper expmap(*this);

    if (isNonSupportedType(D->getType()))
        return nullptr;

    auto loc = fromLoc(D->getLocation());
    auto Ty = D->getType().getDesugaredType(Context);

    if (Ty->isUnionType() || Ty->isStructureType())
    {
        if (auto RT = Ty->getAs<clang::RecordType>()) // dyn_cast because it can also be a typedeftype, which always have names
        {
            auto RD = RT->getDecl();

            if (RD->isAnonymousStructOrUnion()) // NOTE: in union {...} myUnion this will be false, see Decl.h
                return VisitRecordDecl(RD);

            // FIXME how to handle union {...} myUnion? Ignore them for now
            if (!RD->getIdentifier())
                return nullptr;
        }
    }

    auto id = fromIdentifier(D->getIdentifier());
    auto t = fromType(D->getType());

    if (!t)
        return nullptr;

    auto a = new VarDeclaration(loc, id, D, t);

    if (auto Var = dyn_cast<clang::VarDecl>(D))
    {
        if (Var->hasExternalStorage())
            a->storage_class |= STCextern;

        if (Var->getTLSKind() == clang::VarDecl::TLS_Dynamic)
            a->storage_class |= STCtls;

        if (Var->isStaticDataMember())
            a->storage_class |= STCstatic;

        if (Var->isConstexpr() && Var->getAnyInitializer())
        {
            // we avoid initializer expressions except for constexpr variables
            auto e = expmap.fromExpression(Var->getAnyInitializer(),
                                           nullptr, true);
            if (e->op != TOKnull)
            {
                a->init = new ExpInitializer(loc, e);
                a->storage_class |= STCimmutable;
            }
        }
    }

    return oneSymbol(a);
}

Dsymbols *DeclMapper::VisitRecordDecl(const clang::RecordDecl *D, unsigned flags)
{
    auto& Context = calypso.pch.AST->getASTContext();
    auto& S = calypso.pch.AST->getSema();

    if (D->isImplicit())
        return nullptr;

    auto loc = fromLoc(D->getLocation());

    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();  // WARNING: if the definition isn't in the PCH this is going to submit an empty aggregate decl, there could be conflicts if several PCH or modules are used
    bool isDefined = D->isCompleteDefinition();

    auto TND = D->getTypedefNameForAnonDecl();
    bool isPOD = true;

    int anon = 0;
    if (D->isAnonymousStructOrUnion())
    {
        assert(!TND);

        anon = 1;
        if (D->isUnion())
            anon = 2;
    }

    if (!anon && !getIdentifierOrNull(D))
    {
        ::warning(loc, "Value with anon record type, no D equivalent (needs workaround), discarding");
        return nullptr;
    }

    auto CRD = dyn_cast<clang::CXXRecordDecl>(D);
        // NOTE: CXXRecordDecl will disappear in a future version of Clang and only
        // RecordDecl will remain to be used for both C and C++.

    if ((flags & ForceNonPOD) ||
            (CRD && isDefined && !CRD->isPOD()))
        isPOD = false;

    CXXScope.push(D);

    AggregateDeclaration *a;
    if (!anon)
    {
        auto id = getIdentifier(D);

        if (D->isUnion())
        {
            a = new UnionDeclaration(loc, id);
        }
        else if (isPOD)
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
                    auto brt = fromType(B->getType());
                    if (brt->ty == Tvalueof)
                        brt = brt->nextOf();

                    baseclasses->push(new BaseClass(brt,
                                                    toPROT(B->getAccessSpecifier())));
                }
            }

            auto cd = new ClassDeclaration(loc, id, baseclasses, CRD);
            a = cd;
        }

        declMap[D] = a;
    }

    // atm we're sortof mirroring parseAggregate()
    auto members = new Dsymbols;

    if (!isDefined)
        goto Ldeclaration;

    for (auto I = D->field_begin(), E = D->field_end();
            I != E; ++I)
    {
        if (I->getCanonicalDecl() != *I)
            continue;

        auto field = VisitValueDecl(*I);
        if (field)
            members->append(field);
    }

    if (CRD && /* TEMPORARY HACK */ !D->isUnion())
    {
        if (!isPOD && !CRD->isDependentType())
            // Clang declares and defines the implicit default constructor lazily, so do it here
            // before adding methods.
            S.LookupDefaultConstructor(const_cast<clang::CXXRecordDecl *>(CRD));

        for (auto I = CRD->method_begin(), E = CRD->method_end();
            I != E; ++I)
        {
            if (I->getCanonicalDecl() != *I)
                continue;

            auto CCD = dyn_cast<clang::CXXConstructorDecl>(*I);
            if (CCD && CCD->isDefaultConstructor() && isPOD)
                continue;

            // CALYPSO FIXME remove the null check once everything is implemented
            auto fd = VisitFunctionDecl(*I);
            if (fd)
                members->append(fd);
        }
    }

    // Add specific decls: vars, tags, templates, typedefs
#define SPECIFIC_ADD(DECL) \
    typedef clang::DeclContext::specific_decl_iterator<clang::DECL##Decl> DECL##_iterator; \
    for (DECL##_iterator I(D->decls_begin()), E(D->decls_end()); \
                I != E; I++) \
        if (auto s = VisitDecl(*I)) \
            members->append(s);

    SPECIFIC_ADD(Tag)
    SPECIFIC_ADD(Var)
    SPECIFIC_ADD(RedeclarableTemplate)
    SPECIFIC_ADD(TypedefName)

#undef SPECIFIC_ADD

Ldeclaration:
    CXXScope.pop();

    if (anon)
        return oneSymbol(new AnonDeclaration(loc, anon == 2, members));

    a->members = members;

    return oneSymbol(a);
}

Dsymbols *DeclMapper::VisitTypedefNameDecl(const clang::TypedefNameDecl* D)
{
    if (isAnonTagTypedef(D))
        return nullptr;  // the anon tag will be mapped by VisitRecordDecl to an aggregate named after the typedef identifier

    auto loc = fromLoc(D->getLocation());
    auto id = fromIdentifier(D->getIdentifier());
    auto t = fromType(D->getUnderlyingType());

    auto a = new AliasDeclaration(loc, id, t, D);
    return oneSymbol(a);
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

Dsymbols *DeclMapper::VisitFunctionDecl(const clang::FunctionDecl *D)
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.pch.AST->getSema();

    if (isa<clang::CXXConversionDecl>(D))
        return nullptr; // TODO

    auto loc = fromLoc(D->getLocation());

    auto FPT = cast<clang::FunctionProtoType>(D->getType().getTypePtr());
    auto MD = dyn_cast<clang::CXXMethodDecl>(D);
    
    auto tf = FromType(*this).fromTypeFunction(FPT, D);
    if (!tf)
    {
        ::warning(loc, "Discarding %s, has a class value argument or return type",
                            D->getDeclName().getAsString().c_str());
        return nullptr; // FIXME function with unhandled argument types
    }
    assert(tf->ty == Tfunction);

    if (!D->getDeclContext()->isDependentContext())
    {
        auto D_ = const_cast<clang::FunctionDecl*>(D);
        D_->setTrivial(false);  // force its definition and Sema to resolve its exception spec
        S.MarkFunctionReferenced(clang::SourceLocation(), D_);
    }

    StorageClass stc = STCundefined;
    if (MD)
    {
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
            stc &= ~STCoverride; // dtors aren't added to D's VTBLs
    }
    tf->addSTC(stc);
    
    ::FuncDeclaration *fd = nullptr;
    if (auto CD = dyn_cast<clang::CXXConstructorDecl>(D))
    {
        fd = new CtorDeclaration(loc, stc, tf, CD);
    }
    else if (auto DD = dyn_cast<clang::CXXDestructorDecl>(D))
    {
         // Destructors are a special case, Clang can only emit a destructor if it's not trivial.
        // The dtor is checked and added by buildDtor during the semantic pass.
        if (DD->isImplicit())
            return nullptr;

        fd = new DtorDeclaration(loc, stc, Id::dtor, DD);
    }
    else if (D->isOverloadedOperator())
    {
        if (D->isImplicit())
            return nullptr;

        // NOTE: C++ overloaded operators might be virtual, unlike D which are always final (being templates)
        //   Mapping the C++ operator to opBinary()() directly would make D lose info and overriding the C++ method impossible
        auto OO = D->getOverloadedOperator();
        const char *op = clang::getOperatorSpelling(OO);

        Identifier *opIdent;
        bool wrapInTemp = false;

        bool isNonMember = !MD || MD->isStatic();

        auto NumParams = D->getNumParams();
        if (!isNonMember)
            NumParams++;

        bool isUnary = false,
            isBinary = false;

        if (OO == clang::OO_Call)
            opIdent = Id::call;
        else if(OO == clang::OO_Subscript)
            opIdent = Id::index;
        else
        {
            isUnary = NumParams == 1;
            isBinary = NumParams == 2;

            wrapInTemp = true; // except for opAssign

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
                        opIdent = Id::opUnary;
                        break;
                    default:
    //                     ::warning(loc, "Ignoring C++ unary operator%s", clang::getOperatorSpelling(OO));
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
                        opIdent = Id::opBinary;
                        break;
                    case clang::OO_Equal:
                        // D doesn't allow overloading of identity assignment, and since it might still be fundamental
                        // for some types (e.g std::map), map it to another method.
                        // NOTE: C++ assignment operators can't be non-members.
                    {
                        bool isIdentityAssign = false;

                        if (auto RHSLValue = dyn_cast<clang::LValueReferenceType>(
                                        MD->getParamDecl(0)->getType().getDesugaredType(Context).getTypePtr()))
                        {
                            auto LHSType = Context.getTypeDeclType(MD->getParent());
                            auto RHSType = RHSLValue->getPointeeType().withoutLocalFastQualifiers();

                            if (LHSType.getCanonicalType() == RHSType.getCanonicalType())
                                isIdentityAssign = true;
                        }

                        if (isIdentityAssign)
                            opIdent = Lexer::idPool("__opAssign");
                        else
                            opIdent = Id::assign;
                        
                        wrapInTemp = false;
                        break;
                    }
                    case clang::OO_PlusEqual: op = "+"; goto end_opOpAssign;
                    case clang::OO_MinusEqual: op = "-"; goto end_opOpAssign;
                    case clang::OO_StarEqual: op = "*"; goto end_opOpAssign;
                    case clang::OO_SlashEqual: op = "/"; goto end_opOpAssign;
                    case clang::OO_PercentEqual: op = "%"; goto end_opOpAssign;
                    case clang::OO_CaretEqual: op = "^"; goto end_opOpAssign;
                    case clang::OO_AmpEqual: op = "&"; goto end_opOpAssign;
                    case clang::OO_PipeEqual: op = "|"; goto end_opOpAssign;
                    case clang::OO_LessLessEqual: op = "<<"; goto end_opOpAssign;
                    case clang::OO_GreaterGreaterEqual: op = ">>";
end_opOpAssign:
                        opIdent = Id::opOpAssign;
                        break;
                    default:
    //                     ::warning(loc, "Ignoring C++ binary operator%s", clang::getOperatorSpelling(OO));
                        return nullptr;
                }
            }
            else
                return nullptr; // operator new or delete
        }

        Identifier *fullIdent;
        if (wrapInTemp)
        {
            std::string fullName(opIdent->string, opIdent->len);
            fullName += "_";
            fullName += getOperatorName(OO);
            fullIdent = Lexer::idPool(fullName.c_str());
        }
        else
            fullIdent = opIdent;

        // Add the overridable method (or the static function)
        auto a = new Dsymbols;
        fd = new FuncDeclaration(loc, fullIdent, stc, tf, D);
        a->push(fd);

        if (wrapInTemp)
        {
            // Add the opUnary/opBinary/... template declaration
            auto tpl = new TemplateParameters;
            auto dstring = new TypeIdentifier(loc, Id::object);
            dstring->addIdent(Lexer::idPool("string"));
            auto tp_specvalue = new StringExp(loc, const_cast<char*>(op));
            tpl->push(new TemplateValueParameter(loc, Lexer::idPool("s"),
                                                 dstring, tp_specvalue, nullptr));

            auto fwdtf = static_cast<TypeFunction*>(tf->syntaxCopy());

            for (unsigned i = 0; i < D->getNumParams(); i++)
            {
                const char *tp_paramname = (i == 0 && D->getNumParams() == 2) ?
                                    "__lhst" : "__rhst";
                auto tp_paramident = Lexer::idPool(tp_paramname);

                auto tp_spectype = (*tf->parameters)[i]->type;
                tpl->push(new TemplateTypeParameter(loc, tp_paramident,
                                                    tp_spectype, nullptr));

                (*fwdtf->parameters)[i]->type = new TypeIdentifier(loc, tp_paramident);
            }

            auto ffwd = new ::FuncDeclaration(loc, loc, opIdent, STCfinal, fwdtf);

            // build the body of the forwarding function
            auto args = new Expressions;
            args->reserve(fwdtf->parameters->dim);
            for (auto *p: *fwdtf->parameters)
                args->push(new IdentifierExp(loc, p->ident));

            Expression *e = new IdentifierExp(loc, fullIdent);
            e = new CallExp(loc, e, args);

            ffwd->fbody = new ReturnStatement(loc, e);

            auto decldefs = new Dsymbols;
            decldefs->push(ffwd);

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

    return oneSymbol(fd);
}

// NOTE: doesn't return null if the template isn't defined. What we really want is some sort of canonical declaration to refer to for template parameters.
static const clang::ClassTemplateDecl *getDefinition(const clang::ClassTemplateDecl *D)
{
    for (auto RI: D->redecls()) // find the definition if any
    {
        auto I = cast<clang::ClassTemplateDecl>(RI);
        if (I->isThisDeclarationADefinition())
            return I;
    }

    // This is more heuristical than anything else.. I'm not sure yet why templates inside
    // specializations (e.g std::allocator::rebind) do not get defined.
    if (auto MemberTemp = const_cast<clang::ClassTemplateDecl*>(D)->getInstantiatedFromMemberTemplate())
        if (auto MemberDef = getDefinition(MemberTemp))
            return MemberDef;

    return D->getCanonicalDecl();
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
{   TemplateDeclaration *a;

    if (!isa<clang::ClassTemplateDecl>(D) && !isa<clang::TypeAliasTemplateDecl>(D)
         && !isa<clang::FunctionTemplateDecl>(D))
        return nullptr; // temporary

    if (auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D))
    {
        if (FTD->getTemplatedDecl()->isOverloadedOperator())
            return nullptr;

        if (isa<clang::CXXConversionDecl>(FTD->getTemplatedDecl()))
            return nullptr;
    }

    auto loc = fromLoc(D->getLocation());
    auto id = getIdentifier(D);

    if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
        D = CTD = getDefinition(CTD);

    auto tpl = new TemplateParameters;
    auto TPL = D->getTemplateParameters();

    templateParameters.push_back(TPL);

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
        tpl->push(tp);
    }

    auto s = VisitDecl(D->getTemplatedDecl()->getCanonicalDecl());

    if (!s)
        return nullptr;

    auto decldefs = new Dsymbols;
    decldefs->append(s);

    templateParameters.pop_back();

    a = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    return oneSymbol(a);
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

        return Lexer::idPool(OS.str().c_str());
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
        auto valTy = fromType(NTTPD->getType());

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
                        tp_specvalue = expmap.fromAPInt(SpecArg->getAsIntegral());
                        break;
                    case clang::TemplateArgument::NullPtr:
                        tp_specvalue = new NullExp(Loc()/*, fromType(SpecArg->getNullPtrType())*/);
                        break;
                    default:
                        assert(false && "Unsupported template specialization value");
                }

                tp_specvalue = isExpression(FromType(*this).fromTemplateArgument(SpecArg));
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
        id = getIdentifierForTemplateTypeParm(
                cast<clang::TemplateTypeParmType>(TTPD->getTypeForDecl()));

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
                tp_spectype = isType(FromType(*this).fromTemplateArgument(SpecArg));
                assert(tp_spectype);
            }

            if (TTPD->hasDefaultArgument())
                tp_defaulttype = fromType(TTPD->getDefaultArgument());

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
                tp_spectype = isType(FromType(*this).fromTemplateArgument(SpecArg));
                assert(tp_spectype);
            }

            if (TempTemp->hasDefaultArgument())
                tp_defaulttype = FromType(*this).fromTemplateName(
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
    auto CT = getDefinition(D->getSpecializedTemplate());

    // Recreate the scope stack, esp. important for nested template instances
    struct CXXScopeRebuilder
    {
        decltype(CXXScope) &S;
        CXXScopeRebuilder(decltype(S) &S) : S(S) {}

        void build(const clang::Decl *D, bool push = false)
        {
            auto Parent = cast<clang::Decl>(D->getDeclContext());
            if (!isa<clang::TranslationUnitDecl>(Parent))
                build(Parent, true);

            if (push && isa<clang::CXXRecordDecl>(D))
                S.push(D);
        }
    };
    CXXScopeRebuilder(CXXScope).build(D);

    templateParameters.push_back(CT->getTemplateParameters());
    auto a = VisitRecordDecl(D, flags);
    templateParameters.pop_back();

    assert(a->dim);
    return (*a)[0];
}

static const clang::ClassTemplateSpecializationDecl *getDefinition(const clang::ClassTemplateSpecializationDecl *D)
{
    if (auto Definition = D->getDefinition())
        return cast<clang::ClassTemplateSpecializationDecl>(Definition);

    if (auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(D))
        if (auto MemberInst = const_cast<clang::ClassTemplatePartialSpecializationDecl*>(Partial)->getInstantiatedFromMember()) // not the same method name..
            if (auto MemberDef = getDefinition(MemberInst))
                return MemberDef;

    return D;
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
        //
        // C++ template decl lookup doesn't exactly work like D's. For example:
        //
        //    template<typename _Iterator>
        //    struct __iterator_traits<_Iterator, true> {};
        //
        // In C++ you can refer to that partial spec either by __iterator_traits<iterator> or __iterator_traits<iterator, true>
        // whereas in D only __iterator_traits<iterator> works.
        // Furthermore in C++ you may have multiple partial specs taking the same number of seemingly identical parameters.
        // it's not always possible to work around that here.

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
    templateParameters.push_back(TPL);

    for (auto PI = TPL->begin(), PE = TPL->end();
        PI != PE; PI++)
    {
        auto tp = VisitTemplateParameter(*PI, AI);
        tpl->push(tp);

        if (AI) AI++;
    }

    auto decldefs = new Dsymbols;
    auto ad = VisitRecordDecl(D);
    decldefs->append(ad);

    templateParameters.pop_back();

    a = new TemplateDeclaration(loc, id, tpl, decldefs, D);
    return oneSymbol(a);
}

Dsymbols *DeclMapper::VisitEnumDecl(const clang::EnumDecl* D)
{
    if (!D->isCompleteDefinition() && D->getDefinition())
        D = D->getDefinition();

    auto loc = fromLoc(D->getLocation());
    auto ident = getIdentifierOrNull(D);

    Type *memtype = nullptr;
    if (!D->isDependentType())
    {
        auto IntType = D->getIntegerType();
        if (IntType.isNull())
            IntType = D->getPromotionType();

        memtype = fromType(IntType);
    }

    auto e = new EnumDeclaration(loc, ident, memtype, D);

    for (auto ECD: D->enumerators())
    {
        if (!e->members)
            e->members = new Dsymbols;

        auto ident = fromIdentifier(ECD->getIdentifier());
        Expression *value = nullptr;

        if (auto InitE = ECD->getInitExpr())
            value = ExprMapper(*this).fromExpression(InitE);

        auto em = new EnumMember(loc, ident, value, nullptr);
        e->members->push(em);
    }

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
clang::DeclContext::lookup_const_result wideLookup(Loc loc,
                                                   const clang::DeclContext *DC,
                                                   Identifier *id,
                                                   bool lookupInRedecls = true)
{
    auto& AST = calypso.pch.AST;
    auto& Table = AST->getPreprocessor().getIdentifierTable();
    auto& II = Table.get(id->string);

    if (lookupInRedecls)
        if (auto NS = dyn_cast<clang::NamespaceDecl>(DC))
        {
            for (auto Redecl: NS->redecls())
            {
                auto R = wideLookup(loc, Redecl, id, false);
                if (!R.empty())
                    return R;
            }
        }

    typedef clang::DeclContext::specific_decl_iterator<clang::LinkageSpecDecl>
        linkagespec_iterator;
    linkagespec_iterator LSI(DC->decls_begin()), LSE(DC->decls_end());

    do
    {
        auto R = DC->lookup(clang::DeclarationName(&II));

        if (!R.empty())
            return R;

        if (LSI != LSE)
        {
            DC = *LSI;
            LSI++;
        }
        else
            DC = nullptr;

    } while(DC);

    return clang::DeclContext::lookup_const_result();
}

// HACK: The C++ "module loading" works more like a hack at the moment.
// Clang's C++ modules being currently « very experimental and broken », using Clang's module system
// would add many further obstacles to get Calypso working, so I've decided to stick to one big PCH for the
// time being. Once Calypso is working fixing C++ modules should be a TODO.

Module *Module::load(Loc loc, Identifiers *packages, Identifier *id)
{
    auto& Context = calypso.getASTContext();
    auto& S = calypso.pch.AST->getSema();

    S.CurContext = Context.getTranslationUnitDecl(); // HACK? Needed for declaring implicit ctors and dtors

    const clang::DeclContext *DC = Context.getTranslationUnitDecl();
    Package *pkg = rootPackage;

    assert(packages && packages->dim);

    for (size_t i = 1; i < packages->dim; i++)
    {
        Identifier *pid = (*packages)[i];

        auto R = wideLookup(loc, DC, pid);
        if (R.empty())
        {
            ::error(loc, "no C++ package named %s", pid->toChars());
            fatal();
        }

        auto NSN = dyn_cast<clang::NamespaceDecl>(R[0]);
        if (!NSN)
        {
            ::error(loc, "only namespaces can be C++ packages");
            fatal();
        }
        auto ident = fromIdentifier(NSN->getIdentifier());

        pkg = static_cast<Package*>(pkg->symtab->lookup(ident));
        assert(pkg);

        DC = NSN;
    }

    auto m = new Module(moduleName(packages, id).c_str(),
                        id, packages);
    m->members = new Dsymbols;
    m->parent = pkg;
    m->loc = loc;

    DeclMapper mapper(m);

    // HACK « hardcoded modules »
    if (strcmp(id->string, "_") == 0)
    {
        // All non-tag declarations inside the namespace go in _ (this is horrible for C functions of course, this will be fixed by the switch to Clang module system)
        auto NS = dyn_cast<clang::NamespaceDecl>(DC);

        if (!NS)
        {
            warning(Loc(), "package isn't a C++ namespace yet module is '_', ignoring this module for now (testing, will add TU later)");
//             warning(Loc(), "_ identifiers will lose their special meaning after the switch to Clang's module system");
        }
        else
        {
            m->rootDecl = NS->getCanonicalDecl();

            auto I = NS->redecls_begin(),
                    E = NS->redecls_end();

            for (; I != E; ++I)
            {
                DC = *I;

                auto D = DC->decls_begin(),
                        DE = DC->decls_end();

                for (; D != DE; ++D)
                {
                    if (llvm::isa<clang::FunctionDecl>(*D) ||
                            llvm::isa<clang::VarDecl>(*D) ||
                            llvm::isa<clang::TypedefNameDecl>(*D) ||
                            llvm::isa<clang::FunctionTemplateDecl>(*D) ||
                            llvm::isa<clang::TypeAliasTemplateDecl>(*D))
                    {
                        if (auto s = mapper.VisitDecl(*D))
                            m->members->append(s);
                    }
                }
            }
        }
    }
    else
    {
        auto R = wideLookup(loc, DC, id);
        if (R.empty())
        {
            ::error(loc, "no C++ module named %s", id->toChars());
            fatal();
        }

        auto D = R[0];
        if (auto TD = dyn_cast<clang::TypedefNameDecl>(D))
        {
            auto UT = TD->getUnderlyingType().getDesugaredType(Context);
            if (auto RT = dyn_cast<clang::TagType>(UT))
                D = RT->getDecl();
        }

        // Module must be a record or enum
        if (!isa<clang::TagDecl>(D) &&
            !isa<clang::ClassTemplateDecl>(D))
        {
            ::error(loc, "C++ modules have to be records (class/struct, template or not) or enums");
            fatal();
        }

        D = cast<clang::NamedDecl>(D->getCanonicalDecl());
        m->rootDecl = D;

        if (auto s = mapper.VisitDecl(D))
            m->members->append(s);

        // Special case for class template, we need to add explicit specializations to the module as well
        if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
        {
            llvm::SmallVector<clang::ClassTemplatePartialSpecializationDecl *, 2> PS;
            CTD->getPartialSpecializations(PS);

            for (auto PartialSpec: PS)
                if (auto s = mapper.VisitDecl(PartialSpec->getCanonicalDecl()))
                    m->members->append(s);

            for (auto Spec: CTD->specializations())
                if (auto s = mapper.VisitDecl(Spec->getCanonicalDecl()))
                    m->members->append(s);
        }

//         srcFilename = AST->getSourceManager().getFilename(TD->getLocation());
    }
    
    amodules.push_back(m);
    pkg->symtab->insert(m);
    return m;
}

}
