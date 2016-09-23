// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/cpptemplate.h"
#include "driver/cl_options.h"
#include "id.h"
#include "module.h"
#include "template.h"

#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/Type.h"
#include "clang/Lex/ModuleMap.h"
#include "clang/Sema/Sema.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

// Internal Calypso types (unlike TypeValueof which might used by normal D code) essential for template arguments matching.
// D's const being transitive and since Calypso only needs logical const internally, keeping it outside of DMD seemed like the better idea..

class TypePointer : public ::TypePointer
{
public:
    CALYPSO_LANGPLUGIN

    TypePointer(Type *t)
        : ::TypePointer(t) {}

    Type *syntaxCopy(Type *o = nullptr) override
    {
        TypePointer *t;
        if (o)
        {
            assert(isCPP(o) && o->ty == Tpointer);
            t = static_cast<TypePointer*>(o);
        }
        else
            t = new TypePointer(nullptr);

        return ::TypePointer::syntaxCopy(t);
    }

    bool isTransitive() override { return false; }
    bool isMergeable() override { return false; }
    unsigned short sizeType() override { return sizeof(*this); }
};

class TypeReference : public ::TypeReference
{
public:
    CALYPSO_LANGPLUGIN

    bool isRvalueRef;

    TypeReference(Type *t, bool isRvalueRef = false)
        : ::TypeReference(t), isRvalueRef(isRvalueRef) {}

    Type *syntaxCopy(Type *o = nullptr) override
    {
        TypeReference *t;
        if (o)
        {
            assert(isCPP(o) && o->ty == Treference);
            t = static_cast<TypeReference*>(o);
            t->isRvalueRef = isRvalueRef;
        }
        else
            t = new TypeReference(nullptr, isRvalueRef);

        return ::TypeReference::syntaxCopy(t);
    }

    bool isTransitive() override { return false; }
    bool isMergeable() override { return false; }
    unsigned short sizeType() override { return sizeof(*this); }
};

//  There are a few D builtin types that are mapped to several C++ ones, such as wchar_t/dchar <=> wchar_t. Even though they're the same, we have to differentiate them (e.g char_traits<wchar_t>char_traits<char32>) or else two template instances might have different tempdecl while their aggregate member get the same deco
class TypeBasic : public ::TypeBasic
{
public:
    CALYPSO_LANGPLUGIN

    const clang::BuiltinType *T;

    TypeBasic(TY ty, const clang::BuiltinType *T = nullptr);
    unsigned short sizeType() override;
};

TypeBasic::TypeBasic(TY ty, const clang::BuiltinType *T)
    : ::TypeBasic(ty), T(T)
{
}

unsigned short TypeBasic::sizeType()
{
    return sizeof(*this);
}

/***** Builtin type correspondances *****/

void BuiltinTypes::map(clang::CanQualType &CQT, Type* t)
{
    auto T = CQT.getTypePtr()->castAs<clang::BuiltinType>();

    // If a C++ type has more than one D correspondance its Deco needs to be tweaked.
    // Hence NOTE that map() calls need to be ordered by priority.
    if (!toClang.count(t))
    {
        toD[T] = t;
        toClang[t] = T;
    }
    else
    {
        assert(t->isTypeBasic() && !isCPP(t));
        auto c_t = new cpp::TypeBasic(t->ty, T);
        toD[T] = c_t->merge();
        toClang[c_t] = T;
    }
}

void BuiltinTypes::build(clang::ASTContext &Context)
{
    auto& targetInfo = Context.getTargetInfo();

        //===- Void -----------------------------------------------------===//
    map(Context.VoidTy, Type::tvoid);

        //===- Unsigned Types -----------------------------------------------------===//
    map(Context.BoolTy, Type::tbool);     // Clang assumes that bool means unsigned 8 bits
    map(Context.CharTy, Type::tchar);
    map(Context.UnsignedCharTy, Type::tuns8);    // getCharWidth() always returns 8

    map(Context.Char16Ty, Type::twchar);
    map(Context.Char32Ty, Type::tdchar);
    map(Context.UnsignedShortTy, toInt(clang::TargetInfo::UnsignedShort));
    map(Context.UnsignedIntTy, toInt(clang::TargetInfo::UnsignedInt));
    map(Context.UnsignedLongTy, toInt(clang::TargetInfo::UnsignedLong));
    map(Context.UnsignedLongLongTy, toInt(clang::TargetInfo::UnsignedLongLong));
    map(Context.UnsignedInt128Ty, Type::tuns128); // WARNING: a one-to-one correspondance would be safer for template partial specializations
            // NOTE: cent and ucent aren't supported by D and will trigger an error during semantic()

        //===- Signed Types -------------------------------------------------------===//
    map(Context.SignedCharTy, Type::tint8);
    map(Context.ShortTy, toInt(clang::TargetInfo::SignedShort));
    map(Context.IntTy, toInt(clang::TargetInfo::SignedInt));
    map(Context.LongTy, toInt(clang::TargetInfo::SignedLong));
    map(Context.LongLongTy, toInt(clang::TargetInfo::SignedLongLong));
    map(Context.Int128Ty, Type::tint128);

        //===- Floating point types -----------------------------------------------===//
    map(Context.FloatTy, Type::tfloat32);
    map(Context.DoubleTy, Type::tfloat64);
    map(Context.LongDoubleTy, Type::tfloat80);
    map(Context.HalfTy, Type::tfloat32); // FIXME: 16 bits float, map to std.numeric CustomFloat?

        //===- Language-specific types --------------------------------------------===//
    { // nullptr_t needs to be mangled differently from void*
        auto BTVoid = Context.VoidTy.getTypePtr()->castAs<clang::BuiltinType>();
        auto tnull = new cpp::TypeBasic(Tvoid, BTVoid);
        auto tnullptr = tnull->pointerTo();
        map(Context.NullPtrTy, tnullptr);
    }
    map(Context.DependentTy, Type::tnull);  // should work?

    clang::TargetInfo::IntType wcharTy = targetInfo.getWCharType();
    bool isSigned = targetInfo.isTypeSigned(wcharTy);
    Type *twchar_t;

    if (targetInfo.getTypeWidth(wcharTy) == 16)
        twchar_t = isSigned ? Type::tint16 : Type::twchar;
    else
        twchar_t = isSigned ? Type::tint32 : Type::tdchar;
    map(Context.WCharTy, twchar_t);
}

// Most reliable way to determine target-dependent int type correspondances (except for char)
Type *BuiltinTypes::toInt(clang::TargetInfo::IntType intTy)
{
    auto& targetInfo = calypso.getASTContext().getTargetInfo();

    auto width = targetInfo.getTypeWidth(intTy);
    if (clang::TargetInfo::isTypeSigned(intTy))
    {
        switch(width)
        {
            case 8:
                return Type::tint8;
            case 16:
                return Type::tint16;
            case 32:
                return Type::tint32;
            case 64:
                return Type::tint64;
            case 128:
                return Type::tint128;
        }
    }
    else
    {
        switch(width)
        {
            case 8:
                return Type::tuns8;
            case 16:
                return Type::tuns16;
            case 32:
                return Type::tuns32;
            case 64:
                return Type::tuns64;
            case 128:
                return Type::tuns128;
        }
    }

    assert(false && "unexpected int type size");
    return nullptr;
}

/***** Mangler extension for Calypso-specific types *****/

// Fortunately the DMD ::Mangler always call visit((Type *)t); for every type.
// However the visitor pattern is pretty much useless since it's always visit(Type *t) which gets called, ugly but will do for now..

class ManglerExt : public Visitor
{
public:
    OutBuffer *buf;
    Visitor *base;
    bool forEquiv;

    ManglerExt(OutBuffer *buf, bool forEquiv, Visitor *base)
    {
        this->buf = buf;
        this->forEquiv = forEquiv;
        this->base = base;
    }

    void visit(Type *t)
    {
        if (forEquiv)
            return;

        if (t->isTypeBasic())
        {
            buf->writeByte('#');
            buf->writeByte('0' + unsigned(static_cast<cpp::TypeBasic*>(t)->T->getKind()));
            buf->writeByte('#');
        }
        else if (t->ty == Treference)
        {
            auto tref = static_cast<cpp::TypeReference*>(t);
            if (tref->isRvalueRef) {
                buf->writeByte('#');
                buf->writeByte('#');
            }
        }
    }
};

Visitor *LangPlugin::getForeignMangler(OutBuffer *buf, bool forEquiv, Visitor *base)
{
    // FIXME
    static ManglerExt manglerExt(nullptr, false, nullptr);
    manglerExt.buf = buf;
    manglerExt.forEquiv = forEquiv;
    manglerExt.base = base;
    return &manglerExt;
}

/***** Clang -> DMD types *****/

bool isNonSupportedType(clang::QualType T)
{
    auto& Context = calypso.getASTContext();

    // (u)int128_t or any pointer/reference to (TODO: function types as well?)
    auto Pointee = T->getPointeeType();
    while (!Pointee.isNull())
    {
        T = Pointee;
        Pointee = T->getPointeeType();
    }

    if (auto BT = T->getAs<clang::BuiltinType>())
    {
        clang::QualType Builtin(BT, 0);
        if (Builtin == Context.Int128Ty
            || Builtin == Context.UnsignedInt128Ty
            || Builtin == Context.HalfTy)
            return true;
    }

    return false;
}

inline bool isTypeQualifed(Type *t)
{
    return t->ty == Tident || t->ty == Tinstance || t->ty == Ttypeof || t->ty == Treturn;
}

Type *TypeMapper::fromType(const clang::QualType T, Loc loc)
{
    return FromType(*this, loc)(T);
}

TypeMapper::FromType::FromType(TypeMapper &tm, Loc loc, TypeQualified *prefix)
    : tm(tm), loc(loc), prefix(prefix)
{
}

Type *TypeMapper::FromType::operator()(const clang::QualType _T)
{
    if (isNonSupportedType(_T))
        return nullptr;

    auto& Context = calypso.getASTContext();
    clang::QualType T = tm.desugar ? _T.getDesugaredType(Context) : _T;

    Type *t = fromTypeUnqual(T.getTypePtr());

    if (!t)
        return nullptr;

    if (T.isConstQualified())
        t = t->makeConst();

    if (T.isVolatileQualified())
        tm.volatileNumber++;

    // restrict qualifiers are inconsequential

    return t;
}

Type *TypeMapper::FromType::fromType(const clang::QualType T)
{
    return (*this)(T);
}

Type *TypeMapper::FromType::fromTypeUnqual(const clang::Type *T)
{
    if (T->isDependentType())
        isDependent = true;

    if (auto BT = dyn_cast<clang::BuiltinType>(T))
        return fromTypeBuiltin(BT);
    else if (auto FT = dyn_cast<clang::FunctionProtoType>(T))
        return fromTypeFunction(FT);
    else if (auto AT = dyn_cast<clang::AtomicType>(T))
        return fromType(AT->getValueType());
    else if (isa<clang::AutoType>(T)) {
        ::warning(loc, "clang::AutoType unhandled case (FIXME)");
        return nullptr;
    }

    // Purely cosmetic sugar types
    if (auto PT = dyn_cast<clang::ParenType>(T))
        return fromType(PT->desugar());
    else if (auto AT = dyn_cast<clang::AdjustedType>(T))
        return fromType(AT->desugar());

	// MSVC-specific
	if (auto AttrT = dyn_cast<clang::AttributedType>(T))
		return fromType(AttrT->getEquivalentType());

#define TYPEMAP(Ty) \
    if (auto Ty##T = dyn_cast<clang::Ty##Type>(T)) \
        return fromType##Ty(Ty##T);

    TYPEMAP(Complex)
    TYPEMAP(Typedef)
    TYPEMAP(Enum)
    TYPEMAP(Record)
    TYPEMAP(MemberPointer)
    TYPEMAP(Elaborated)
    TYPEMAP(TemplateSpecialization)
    TYPEMAP(TemplateTypeParm)
    TYPEMAP(SubstTemplateTypeParm)
    TYPEMAP(InjectedClassName)
    TYPEMAP(DependentName)
    TYPEMAP(DependentTemplateSpecialization)
    TYPEMAP(Decltype)
    TYPEMAP(TypeOfExpr)
    TYPEMAP(PackExpansion)
    TYPEMAP(Vector)
    TYPEMAP(Array)
    TYPEMAP(UnaryTransform)
#undef TYPEMAP

    // Pointer and reference types
    auto Pointer = dyn_cast<clang::PointerType>(T);
    auto Reference = dyn_cast<clang::ReferenceType>(T);
    auto BlockPointer = dyn_cast<clang::BlockPointerType>(T); // OS X extension

    if (Pointer || Reference || BlockPointer)
    {
        auto pointeeT = Reference ? Reference->getPointeeTypeAsWritten() :
                (Pointer ? Pointer->getPointeeType() : BlockPointer->getPointeeType());
        auto pt = fromType(pointeeT);
        if (!pt)
            return nullptr;

        Type *t;
        if (Pointer || BlockPointer)
            t = new TypePointer(pt);
        else
            t = new TypeReference(pt, isa<clang::RValueReferenceType>(Reference));
        return t->merge();
    }

    llvm::llvm_unreachable_internal("Unrecognized C++ type");
}


Type *TypeMapper::FromType::fromTypeBuiltin(const clang::BuiltinType *T)
{
    auto t = calypso.builtinTypes.toD[T];

    assert(t && "missing built-in type correspondance");
    return t;
}

Type *TypeMapper::FromType::fromTypeComplex(const clang::ComplexType *T)
{
    auto& Context = calypso.getASTContext();
    auto dT = T->desugar();

    if (dT == Context.FloatComplexTy)
        return Type::tcomplex32;
    else if (dT == Context.DoubleComplexTy)
        return Type::tcomplex64;
    else if (dT == Context.LongDoubleComplexTy)
        return Type::tcomplex80;

    assert(false && "unknown complex number type");
    return nullptr;
}

Type* TypeMapper::FromType::fromTypeVector(const clang::VectorType* T)
{
    auto t = fromType(T->getElementType());
    if (!t)
        return nullptr;
    auto dim = new IntegerExp(T->getNumElements());

    return new TypeVector(loc, new TypeSArray(t, dim));
}

Type* TypeMapper::FromType::fromTypeArray(const clang::ArrayType* T)
{
    auto t = fromType(T->getElementType());
    if (!t)
        return nullptr;

    if (auto CAT = dyn_cast<clang::ConstantArrayType>(T))
    {
        auto dim = new IntegerExp(CAT->getSize().getLimitedValue());
        return new TypeSArray(t, dim);
    }
    else if (auto DSAT = dyn_cast<clang::DependentSizedArrayType>(T))
    {
        auto dim = ExprMapper(tm).fromExpression(DSAT->getSizeExpr());
        return new TypeSArray(t, dim);
    }
    else if (isa<clang::IncompleteArrayType>(T))
    {
        return new TypePointer(t);
    }

    llvm::llvm_unreachable_internal("Unrecognized C++ array type");
}

// MSVC-specific(?) __underlying_type intrinsic returning the underlying integral type of an enum
Type* TypeMapper::FromType::fromTypeUnaryTransform(const clang::UnaryTransformType* T)
{
    auto Underlying = T->getUnderlyingType();
    if (!Underlying.isNull())
        return fromType(Underlying);

    return fromType(T->getBaseType()); // Underlying may be null if T is dependent
}

template<bool wantTuple>
  Objects* TypeMapper::FromType::fromTemplateArgument(const clang::TemplateArgument* Arg,
                const clang::NamedDecl *Param)
{
    ExprMapper expmap(tm);
    auto tiargs = new Objects;

    std::function<void(const clang::TemplateArgument*, Objects*)>
        addArg = [&] (const clang::TemplateArgument* Arg, Objects* tuple = nullptr)
    {
        RootObject *tiarg = nullptr;

        switch (Arg->getKind())
        {
            case clang::TemplateArgument::Expression:
                tiarg = expmap.fromExpression(Arg->getAsExpr());
                break;
            case clang::TemplateArgument::Integral:
            {
                auto e = expmap.fromAPInt(loc, Arg->getAsIntegral());

                if (auto NTTP = llvm::dyn_cast_or_null<clang::NonTypeTemplateParmDecl>(Param))
                    e = expmap.fixIntegerExp(static_cast<IntegerExp*>(e), NTTP->getType());

                tiarg = e;
                break;
            }
            case clang::TemplateArgument::NullPtr:
                tiarg = new NullExp(loc/*, fromType(Arg->getNullPtrType())*/);
                break;
            case clang::TemplateArgument::Type:
                tiarg = FromType(tm, loc)(Arg->getAsType());
                break;
            case clang::TemplateArgument::Template:
                tiarg = fromTemplateName(Arg->getAsTemplate());
                break;
            case clang::TemplateArgument::Pack:
                if (wantTuple)
                {
                    auto tup = new Tuple;
                    for (auto& PackArg: Arg->pack_elements())
                        addArg(&PackArg, &tup->objects);
                    tiarg = tup;
                    break;
                }
                else
                {
                    for (auto& PackArg: Arg->pack_elements())
                        addArg(&PackArg, nullptr);
                    return;
                }
            default:
                assert(false && "Unsupported template arg kind");
        }

        assert((tiarg || Arg->getKind() == clang::TemplateArgument::Type) && "Template argument not supported");
        if (tuple)
            tuple->push(tiarg);
        else
            tiargs->push(tiarg);
    };

    addArg(Arg, nullptr);
    return tiargs;
}

template Objects *TypeMapper::FromType::fromTemplateArgument<false>(const clang::TemplateArgument *Arg,
                                                                                const clang::NamedDecl *Param);
template Objects *TypeMapper::FromType::fromTemplateArgument<true>(const clang::TemplateArgument *Arg,
                                                                                const clang::NamedDecl *Param);

template<bool wantTuple>
  Objects* TypeMapper::FromType::fromTemplateArguments(const clang::TemplateArgument *First,
                                        const clang::TemplateArgument *End,
                                        const clang::TemplateParameterList *ParamList)
{
    auto tiargs = new Objects;
    auto Param = ParamList ? ParamList->begin() : nullptr;

    for (auto Arg = First; Arg != End; Arg++)
    {
        auto P = Param ? *Param : nullptr;
        auto arg = fromTemplateArgument<wantTuple>(Arg, P);
        assert(arg->dim || Arg->getKind() == clang::TemplateArgument::Pack);
        tiargs->append(arg);

        if (ParamList)
            Param++;
    }

    return tiargs;
}

template
  Objects* TypeMapper::FromType::fromTemplateArguments<true>(const clang::TemplateArgument *First,
                                        const clang::TemplateArgument *End,
                                        const clang::TemplateParameterList *ParamList);

Objects *TypeMapper::fromTemplateArguments(Loc loc, const clang::TemplateArgumentList *List,
                                           const clang::TemplateParameterList *ParamList)
{
    auto Array = List->asArray();
    auto First = Array.begin(), End = Array.end();
    return FromType(*this, loc).fromTemplateArguments(First, End, ParamList);
}

class ScopeChecker // determines if a C++ decl is "scopingly" equivalent to another's
{
public:
    const clang::Decl *Scope, *Pattern = nullptr;
    bool MemberTemplate; // go back to the member template

    const clang::Decl *getScope(const clang::Decl *D)
    {
        auto Result = D;
        if (auto ClassTemp = dyn_cast<clang::ClassTemplateDecl>(D))
            Result = ClassTemp->getTemplatedDecl();
        else if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
            Result = FuncTemp->getTemplatedDecl();
        return Result->getCanonicalDecl();
    }

    const clang::Decl *getPattern(const clang::Decl *D)
    {
        const clang::Decl *Result = nullptr;

        auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D);
        auto Func = dyn_cast<clang::FunctionDecl>(D);
        if (ClassSpec && !ClassSpec->isExplicitSpecialization())
        {
            auto Temp = getSpecializedDeclOrExplicit(ClassSpec);
            if (auto ClassTemp = dyn_cast<clang::ClassTemplateDecl>(Temp))
                Result = ClassTemp->getTemplatedDecl();
            else
                Result = cast<clang::ClassTemplatePartialSpecializationDecl>(Temp);
        }
        else if (Func && Func->getPrimaryTemplate() &&
                    Func->getTemplateSpecializationKind() != clang::TSK_ExplicitSpecialization)
            Result = Func->getPrimaryTemplate()->getTemplatedDecl();

        if (Result)
            return Result->getCanonicalDecl();
        else
            return nullptr;
    }
    
    const clang::Decl *toMemberTemplate(const clang::Decl *D)
    {
        const clang::RedeclarableTemplateDecl *Temp = nullptr;
        
        if (auto Class = dyn_cast<clang::CXXRecordDecl>(D))
            Temp = Class->getDescribedClassTemplate();
        else if (auto Func = dyn_cast<clang::FunctionDecl>(D))
            Temp = Func->getDescribedFunctionTemplate();
        
        if (Temp)
            if (auto MemberTemp = Temp->getInstantiatedFromMemberTemplate())
                return MemberTemp->getTemplatedDecl();
            
          return D;
    }

    ScopeChecker(const clang::Decl *D, bool MemberTemplate = false)
            : MemberTemplate(MemberTemplate)
    {
        Scope = getScope(D);
        Pattern = getPattern(D);
        
        if (MemberTemplate)
        {
            Scope = toMemberTemplate(Scope);
            if (Pattern)
                Pattern = toMemberTemplate(Pattern);
        }
    }

    bool operator()(const clang::Decl *D, bool checkBases = true)
    {
        auto CanonScope = getScope(D);
        if (MemberTemplate)
            CanonScope = toMemberTemplate(CanonScope);
        if (CanonScope == Scope ||
                (Pattern && CanonScope == Pattern))
            return true;

        auto Record = dyn_cast<clang::CXXRecordDecl>(Scope);
        if (checkBases && Record && Record->getDefinition())
        {
            Record = Record->getDefinition();
            for (auto& B: Record->bases())
            {
                auto BRT = B.getType()->getAs<clang::RecordType>();

                if (!BRT)
                    continue;

                auto BRD = BRT->getDecl();
                if (ScopeChecker(BRD)(D))
                    return true;
            }
        }

        return false;
    }

    bool isDirectParent(const clang::Decl *D)
    {
        auto Parent = cast<clang::Decl>(
                        getDeclContextNonLinkSpec(D));
        return operator()(Parent, false);
    }

    bool extended(const clang::Decl *D)
    {
        if (operator()(D))
            return true;

        if (auto ClassPattern = llvm::dyn_cast_or_null<clang::CXXRecordDecl>(Pattern))
            if (auto ClassTemplate = ClassPattern->getDescribedClassTemplate())
                if (auto MemberTemplate = ClassTemplate->getInstantiatedFromMemberTemplate())
                    return ScopeChecker(MemberTemplate)(D);

        return false;
    }
};

// Messy... and also used for expression mapping, because TypeQualified's purpose is very similar
// to expressions. Could TypeQualified be merged with DotIdExp & co?
class TypeQualifiedBuilder
{
public:
    TypeMapper::FromType &from;
    TypeMapper &tm;

    TypeQualifiedBuilderOpts options;

    ScopeChecker RootEquals;
    const clang::NamedDecl *TopDecl;
    const clang::TemplateArgument *TopTempArgBegin,
        *TopTempArgEnd;

    void add(TypeQualified *&tqual,
                  RootObject *o);
    void addIdent(TypeQualified *&tqual,
                  Identifier *ident);
    void addInst(TypeQualified *&tqual,
                  TemplateInstance *tempinst);

    void pushInst(TypeQualified *&tqual,
                RootObject *o,
                const clang::TemplateDecl *Temp,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd,
                clang::NamedDecl *Spec = nullptr);

    RootObject *getIdentOrTempinst(const clang::Decl *D);

    TypeQualifiedBuilder(TypeMapper::FromType &from, const clang::Decl* Root,
        const clang::NamedDecl *TopDecl = nullptr,
        const clang::TemplateArgument *TempArgBegin = nullptr,
        const clang::TemplateArgument *TempArgEnd = nullptr,
        TypeQualifiedBuilderOpts options = TQ_None)
        : from(from), tm(from.tm), options(options), RootEquals(Root),
          TopDecl(TopDecl),
          TopTempArgBegin(TempArgBegin),
          TopTempArgEnd(TempArgEnd)
    {
    }

    TypeQualified *get(const clang::Decl *D);
};

void TypeQualifiedBuilder::add(TypeQualified *&tqual, RootObject *o)
{
    if (o->dyncast() == DYNCAST_IDENTIFIER)
        addIdent(tqual, static_cast<Identifier*>(o));
    else
        addInst(tqual, static_cast<TemplateInstance*>(o));
}

void TypeQualifiedBuilder::addIdent(TypeQualified *&tqual,
                                    Identifier *ident)
{
    if (!tqual)
        tqual = new TypeIdentifier(from.loc, ident);
    else
        tqual->addIdent(ident);
}

void TypeQualifiedBuilder::addInst(TypeQualified *&tqual,
                                   TemplateInstance *tempinst)
{
    if (!tqual)
        tqual = new TypeInstance(from.loc, tempinst);
    else
        tqual->addInst(tempinst);
}

void TypeQualifiedBuilder::pushInst(TypeQualified *&tqual,
                RootObject *o,
                const clang::TemplateDecl *Temp,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd,
                clang::NamedDecl *Spec)
{
    auto loc = fromLoc(Spec ? Spec->getLocation() : Temp->getLocation());
    auto tiargs = from.fromTemplateArguments(ArgBegin, ArgEnd,
            Temp->getTemplateParameters());

    cpp::TemplateInstance *tempinst;
    if (o->dyncast() == DYNCAST_IDENTIFIER)
    {
        auto ident = static_cast<Identifier*>(o);
        tempinst = new cpp::TemplateInstance(loc, ident);
        tempinst->tiargs = tiargs;
    }
    else // templated overloaded operator
    {
        tempinst = static_cast<cpp::TemplateInstance*>(o);
        tempinst->tiargs->append(tiargs);
    }

    // NOTE: To reduce DMD -> Clang translations to a minimum we don't instantiate ourselves whenever possible,
    // i.e when the template instance is already declared or defined in the PCH. If it's only declared, we tell Sema to
    // complete its instantiation and determine whether it is non-polymorphic or not.
    if (Spec && !from.isDependent)
        tempinst->Inst = Spec;

    addInst(tqual, tempinst);
}

RootObject *TypeQualifiedBuilder::getIdentOrTempinst(const clang::Decl *D)
{
    auto Named = dyn_cast<clang::NamedDecl>(D);
    if (!Named)
        return nullptr;

    if (options & TQ_OverOpFullIdent)
        return getExtendedIdentifierOrNull(Named, tm);

    SpecValue spec(tm); // overloaded operator
    auto ident = getIdentifierOrNull(Named, &spec);

    if (spec && !(options & TQ_OverOpSkipSpecArg))
    {
        auto loc = fromLoc(D->getLocation());
        auto tempinst = new cpp::TemplateInstance(loc, ident);
        tempinst->tiargs = new Objects;
        tempinst->tiargs->push(spec.toTemplateArg(from.loc));
        return tempinst;
    }

    return ident;
}

TypeQualified *TypeQualifiedBuilder::get(const clang::Decl *D)
{
    TypeQualified *tqual;

    if (from.prefix)
        tqual = from.prefix; // special case where the prefix has already been determined from a NNS
    else if (!isa<clang::TranslationUnitDecl>(D) && RootEquals(D))
        tqual = nullptr;
    else
    {
        auto LeftMost = GetNonNestedContext(D);
        ScopeChecker LeftMostCheck(LeftMost);

        if (LeftMostCheck(D))  // we'll need a fully qualified type
        {
            if (from.tm.cppPrefix)
                tqual = new TypeIdentifier(from.loc, Id::empty); // start with the module scope operator . to guarantee against collisions
            else
                tqual = nullptr;

            // build a fake import
            if (auto im = tm.AddImplicitImportForDecl(from.loc, TopDecl, true))
            {
                if (im->aliasId)
                    addIdent(tqual, im->aliasId);
                else
                {
                    for (size_t i = from.tm.cppPrefix ? 0 : 1; i < im->packages->dim; i++)
                        addIdent(tqual, (*im->packages)[i]);
                    addIdent(tqual, im->id);
                }
            }
            // if no Import was returned D is part of the module being mapped

            if (isa<clang::NamespaceDecl>(D) || isa<clang::TranslationUnitDecl>(D))
                return tqual;
        }
        else
        {
            auto Parent = cast<clang::Decl>(
                        getDeclContextNamedOrTU(D));
            tqual = get(Parent);
        }
    }

    auto DC = getDeclContextNamedOrTU(D);
    if (isa<clang::EnumConstantDecl>(D) &&
            from.prefix && DC->isTransparentContext())
        add(tqual, getIdentOrTempinst(cast<clang::NamedDecl>(DC))); // if the enum constant is prefixed in the DeclRef and the enum isn't a C++11 static enum, append its name
            // FIXME: generalize for any transparent context?

    auto o = getIdentOrTempinst(D);
    if (!o)
        return tqual;

    clang::NamedDecl *Spec = nullptr;
    const clang::TemplateDecl *SpecTemp = nullptr;
    llvm::ArrayRef<clang::TemplateArgument> TempArgs;

    llvm::SmallVector<clang::TemplateArgument, 4> ArgsArray; // temporary array used for function specs only

    if (auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
    {
        if (!tm.isInjectedScopeName(D))
        {
            Spec = const_cast<clang::ClassTemplateSpecializationDecl*>(ClassSpec);
            SpecTemp = ClassSpec->getSpecializedTemplate();
            TempArgs = ClassSpec->getTemplateArgs().asArray();
        }
    }
    else if (auto Func = dyn_cast<clang::FunctionDecl>(D)) // functions will always be at the top
    {
        auto ExplicitArgs = Func->getTemplateSpecializationArgsAsWritten();

        if (ExplicitArgs)
        {
            Spec = const_cast<clang::FunctionDecl*>(Func);
            SpecTemp = Func->getPrimaryTemplate();

            for (unsigned i = 0; i < ExplicitArgs->NumTemplateArgs; i++)
                ArgsArray.push_back((*ExplicitArgs)[i].getArgument());
            TempArgs = ArgsArray;
        }
        // NOTE: Function specs without explicit arguments will be mapped to Identifier
        // and that's okay (+ avoids argument deduction).
    }

    auto Temp = dyn_cast<clang::TemplateDecl>(D);
    if (Temp && TopTempArgBegin)
    {
        pushInst(tqual, o, Temp, TopTempArgBegin, TopTempArgEnd);
        TopTempArgBegin = TopTempArgEnd = nullptr;  // e.g there could be multiple TypeAliasTemplateDecl in the same qualified type
    }
    else if (Spec)
    {
        pushInst(tqual, o, SpecTemp, TempArgs.begin(), TempArgs.end(), Spec);
    }
    else
        add(tqual, o);

    return tqual;
}

const clang::Decl *TypeMapper::GetRootForTypeQualified(clang::NamedDecl *D)
{
    clang::DeclarationName Name = D->getDeclName();
    decltype(CXXScope) ScopeStack(CXXScope);

    if (auto Tag = llvm::dyn_cast<clang::TagDecl>(D))
        if (auto Typedef = Tag->getTypedefNameForAnonDecl())
            Name = Typedef->getDeclName();

    if (Name.isEmpty()) // might be an anonymous enum decl for fromExpressionDeclRef
            // TODO: check that this doesn't happen when called from TypeMapper would be more solid
        return nullptr;

    while (!ScopeStack.empty())
    {
        auto ScopeDecl = ScopeStack.top();
        ScopeStack.pop();
        ScopeChecker ScopeDeclCheck(ScopeDecl);

        assert(ScopeDecl->getCanonicalDecl() != D->getCanonicalDecl()); // should already be handled

        const clang::Decl *DI = D, *LastNamedDI = D;
        while(!isa<clang::TranslationUnitDecl>(DI))
        {
            if (ScopeDeclCheck.isDirectParent(DI))
                return LastNamedDI;

            DI = cast<clang::Decl>(
                    getDeclContextNonLinkSpec(DI));

            if (auto Named = dyn_cast<clang::NamedDecl>(DI))
                if (!Named->getDeclName().isEmpty())
                    LastNamedDI = DI;
        }

//         bool fullyQualify = false;
//
//         auto ScopeDC = cast<clang::DeclContext>(ScopeDecl);
//         auto LookupResult = ScopeDC->lookup(Name);
//         for (auto Decl: LookupResult)
//         {
//             if (Decl->isImplicit())
//                 continue;
//
//             fullyQualify = true;
//             break;
//         }
//
//         if (auto Named = dyn_cast<clang::NamedDecl>(ScopeDecl))
//             if (clang::DeclarationName::compare(Named->getDeclName(), Name) == 0)
//                 fullyQualify = true;
//
        // NOTE: this isn't enough to check for collisions, imported symbols might collide too
        // and implicit imports aren't known until the end of the mapping.
//
//         if (fullyQualify)
//         {
//             Root = D->getTranslationUnitDecl(); // to avoid name collisions, we fully qualify the type
//             goto LrootDone;
//         }
    }

    auto DK = GetImplicitImportKeyForDecl(D);
    auto ModDC = GetNonNestedContext(D);

    if (mod && DK == mod->rootKey)
    {
        if (isa<clang::TagDecl>(ModDC))
            return ModDC;

        // else ModDC is a namespace/TU and we need to return the first decl to its right
        auto Root = D;
        while (D->getCanonicalDecl() != ModDC->getCanonicalDecl())
        {
            auto DC = getDeclContextNamedOrTU(D);
            if (DC->isTranslationUnit())
                return D->getTranslationUnitDecl();

            Root = D;
            D = cast<clang::NamedDecl>(const_cast<clang::DeclContext*>(DC));
        }
        return Root;
    }

    return D->getTranslationUnitDecl();
}

static TypeQualified *fromInjectedClassName(Loc loc)
{
    return new TypeTypeof(loc, new ThisExp(loc));
}

TypeQualified *TypeMapper::FromType::typeQualifiedFor(clang::NamedDecl *D,
                        const clang::TemplateArgument *ArgBegin, const clang::TemplateArgument *ArgEnd,
                        TypeQualifiedBuilderOpts options)
{
    if (tm.isInjectedClassName(D))
        return fromInjectedClassName(loc); // in the following :
                    //      class time_duration { public: uint seconds() { return 60; } }
                    //      class seconds : time_duration { public: void foo(seconds *b) {} }
                    // searching "seconds" in the derived class will return the function.
                    // seconds needs to be replaced by « typeof(this) ».

    if (tm.isInjectedScopeName(D))
        return new TypeIdentifier(fromLoc(D->getLocation()), getIdentifier(D));

    auto Root = tm.GetRootForTypeQualified(D);
    if (!Root)
        return nullptr; // FIXME struct {} Val;

    tm.AddImplicitImportForDecl(loc, D);
    return TypeQualifiedBuilder(*this, Root, D, ArgBegin, ArgEnd, options).get(D);
}

Type *TypeMapper::FromType::typeSubstOrQualifiedFor(clang::NamedDecl *D,
                const clang::TemplateArgument *ArgBegin, const clang::TemplateArgument *ArgEnd)
{
    if (!ArgBegin)
        if (auto subst = tm.trySubstitute(D)) // HACK for correctTiargs
            return subst;

    return typeQualifiedFor(D, ArgBegin, ArgEnd);
}

Type *TypeMapper::trySubstitute(const clang::Decl *D)
{
    if (!substsyms)
        return nullptr;

    for (auto s: *substsyms)
    {
#define SUBST(Kind, Sym) \
        else if (s->is##Kind##Declaration())  \
        { \
            auto Known = static_cast<cpp::Kind##Declaration*>(s)->Sym; \
            if (Known->getCanonicalDecl() != D->getCanonicalDecl()) \
                continue; \
            return new ::Type##Kind(static_cast<Kind##Declaration*>(s)); \
        }

        if (0) ;
        SUBST(Struct, RD)
        SUBST(Class, RD)
        SUBST(Enum, ED)
        else if (s->isTemplateDeclaration())
            continue;
        else
            assert(false && "Unexpected symbol kind");

#undef SUBST
    }

    return nullptr;
}

Type* TypeMapper::FromType::fromTypeTypedef(const clang::TypedefType* T)
{
    auto Typedef = T->getDecl();
    if (isAnonTagTypedef(Typedef) || isSameNameTagTypedef(Typedef) ||
                getDeclContextNamedOrTU(Typedef)->isTranslationUnit())  // temporary HACK to avoid importing "_" just because of typedefs (eg size_t)
        return fromType(Typedef->getUnderlyingType());

    return typeSubstOrQualifiedFor(Typedef);
}

Type* TypeMapper::FromType::fromTypeEnum(const clang::EnumType* T)
{
    return typeSubstOrQualifiedFor(T->getDecl());
}

Type *TypeMapper::FromType::fromTypeRecord(const clang::RecordType *T)
{
    return typeSubstOrQualifiedFor(T->getDecl());
}

Type *TypeMapper::FromType::fromTypeMemberPointer(const clang::MemberPointerType *T)
{
    // Rarely used feature of C++, see [expr.mptr.oper]
    if (T->isMemberDataPointer())
        return Type::tptrdiff_t;
    else
    {
        auto FPT = cast<clang::FunctionProtoType>(
                        withoutNonAliasSugar(T->getPointeeType()).getTypePtr());
        auto ft = FromType(tm, loc).fromTypeFunction(FPT);
        auto tc = FromType(tm, loc).fromTypeUnqual(T->getClass());

        // in the Itanium ABI a member func pointer is a pair of ptrdiff_t { ptr; thisadj; }
        // NOTE: making those types' semantics dependent on the C++ ABI was a conscious choice
        auto ti = new ::TemplateInstance(loc, Identifier::idPool("__cpp_member_funcptr"));
        ti->tiargs = new Objects;
        ti->tiargs->push(ft); // we need to remember the function type and the parent class, in case we have to send the type back to Clang
        ti->tiargs->push(tc);

        auto t = new TypeIdentifier(loc, Identifier::idPool("object"));
        t->addInst(ti);
        return t;
    }
}

Type *TypeMapper::FromType::fromTypeElaborated(const clang::ElaboratedType *T)
{
    // NOTE: Why must we sometimes respect NestedNameSpecifiers? Because of this situation:
    //     template<typename _Iterator>
    //       class reverse_iterator
    //       : public iterator<typename iterator_traits<_Iterator>::iterator_category>
    //
    // When mapping the template DMD will add an import to iterator_traits, but when
    // the instance of iterator will be mapped, iterator_category will have the *base* class
    // of the specialization of iterator_traits, __iterator_traits as its parent decl context,
    // I.e __iterator_traits::iterator_category
    // But iterator isn't aware of __iterator_traits so lookup error.
    // The NNS will always be known, so use it.

    TypeQualified *tqual = nullptr;
    if (auto NNS = T->getQualifier())
    {
        if (NNS->getKind() == clang::NestedNameSpecifier::TypeSpec ||
                NNS->getKind() == clang::NestedNameSpecifier::TypeSpecWithTemplate)
            tqual = fromNestedNameSpecifier(NNS);
    }

    return FromType(tm, loc, tqual)(T->getNamedType());
}

namespace
{
// Checks (roughly) if a template specialization type will lead to recursive instantiation of a class template
// See for example boost/mpl/aux_/integral_wrapper.hpp's "next" and "prev" constants. In C++ they're instantiated lazily
// so it's not a problem, in D they make DMD instantiate the template endlessly.
// The workaround is to discard the decls and cross fingers that they're aren't required elsewhere.

typedef unsigned RCResult;
const RCResult RC_Not = 0;
const RCResult RC_Literal = 1 << 0;
const RCResult RC_Dependent = 1 << 1;
const RCResult RC_RecursiveMaybe = RC_Literal | RC_Dependent;

class RecursiveInstChecker : public clang::ConstStmtVisitor<RecursiveInstChecker, RCResult>
{
public:
    const clang::CXXRecordDecl *InjectedName;
    RecursiveInstChecker(const clang::CXXRecordDecl *InjectedName)
            : InjectedName(InjectedName) {}

    RCResult VisitStmt(const clang::Stmt *) { return RC_Not; }

#define FALLTHROUGH(CLASS, SUBEXPR) \
    RCResult Visit##CLASS(const clang::CLASS *E) { return Visit(E->SUBEXPR()); }

    FALLTHROUGH(CXXStaticCastExpr, getSubExpr)
    FALLTHROUGH(ParenExpr, getSubExpr)
    FALLTHROUGH(CXXDefaultArgExpr, getExpr)
    FALLTHROUGH(ImplicitCastExpr, getSubExpr)
#undef FALLTHROUGH

    RCResult VisitSubstNonTypeTemplateParmExpr(const clang::SubstNonTypeTemplateParmExpr *E) {
        return RC_Dependent;
    }

    RCResult VisitDeclRefExpr(const clang::DeclRefExpr *E) {
        auto Var = dyn_cast<clang::VarDecl>(E->getDecl());
        if (Var && cast<clang::Decl>(Var->getDeclContext())->getCanonicalDecl()
                            == InjectedName->getCanonicalDecl())
            if (auto Init = Var->getAnyInitializer())
                return Visit(Init);

        return RC_Not;
    }

    RCResult VisitIntegerLiteral(const clang::IntegerLiteral *E) {
        return RC_Literal;
    }

    RCResult VisitUnaryOperator(const clang::UnaryOperator *E) {
        switch (E->getOpcode())
        {
            case clang::UO_PostInc:
            case clang::UO_PostDec:
            case clang::UO_PreInc:
            case clang::UO_PreDec:
                if (Visit(E->getSubExpr()) & RC_Dependent)
                    return RC_RecursiveMaybe;
                break;
            case clang::UO_Plus:
            case clang::UO_Minus:
                return Visit(E->getSubExpr());
            default:
                break;
        }
        return RC_Not;
    }

    RCResult VisitBinaryOperator(const clang::BinaryOperator *E) {
        RCResult Result = RC_Not;

        switch (E->getOpcode())
        {
            case clang::BO_Add:
            case clang::BO_Sub:
            case clang::BO_Mul:
            case clang::BO_Shl:
            case clang::BO_Shr:
                Result = Visit(E->getLHS()) | Visit(E->getRHS());
                break;
            default:
                break;
        }
        return Result;
    }
};

}

bool TypeMapper::isRecursivelyInstantiated(const clang::TemplateName Name,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd)
{
    auto Temp = llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(Name.getAsTemplateDecl());
    if (!Temp)
        return false;
    auto Pattern = Temp->getTemplatedDecl();

    const clang::CXXRecordDecl *InjectedName = nullptr;
    decltype(CXXScope) ScopeStack(CXXScope);
    while (!ScopeStack.empty())
    {
        auto ScopeDecl = ScopeStack.top();
        ScopeStack.pop();

        auto ScopePattern = ScopeDecl;
        if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(ScopePattern))
            ScopePattern = Spec->getSpecializedTemplate()->getTemplatedDecl();

        if (Pattern->getCanonicalDecl() ==
                ScopePattern->getCanonicalDecl())
        {
            InjectedName = dyn_cast<clang::CXXRecordDecl>(ScopeDecl);
            break;
        }
    }

    if (InjectedName)
    {
        int packRecurse = 0;

        for (auto Arg = ArgBegin; Arg != ArgEnd; Arg++)
        {
            switch (Arg->getKind())
            {
                case clang::TemplateArgument::Expression:
                {
                    auto E = Arg->getAsExpr();

                    if (RecursiveInstChecker(InjectedName).Visit(E) == RC_RecursiveMaybe)
                        return true;

                    auto SNTP = dyn_cast<clang::SubstNonTypeTemplateParmExpr>(E);
                    if (SNTP && SNTP->getParameter() && SNTP->getParameter()->isParameterPack())
                        packRecurse |= 2;
                    else
                        packRecurse |= 1;
                }
                case clang::TemplateArgument::Type:
                    break;
                default:
                    break;
            }
        }

        if (packRecurse == 3)
            return true;
    }

    return false;
}

TypeQualified *TypeMapper::FromType::fromTemplateName(const clang::TemplateName Name,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd)
{
    if (ArgBegin && tm.isRecursivelyInstantiated(Name, ArgBegin, ArgEnd))
        return nullptr;

    Identifier *tempIdent;

    switch (Name.getKind())
    {
        case clang::TemplateName::Template:
            return typeQualifiedFor(Name.getAsTemplateDecl(), ArgBegin, ArgEnd);

        case clang::TemplateName::QualifiedTemplate:
        {
            TypeQualified *tqual = nullptr;
            auto NNS = Name.getAsQualifiedTemplateName()->getQualifier();

            if (NNS->getKind() == clang::NestedNameSpecifier::TypeSpec ||
                    NNS->getKind() == clang::NestedNameSpecifier::TypeSpecWithTemplate)
                tqual = fromNestedNameSpecifier(NNS);

            return FromType(tm, loc, tqual).typeQualifiedFor(Name.getAsTemplateDecl(), ArgBegin, ArgEnd);
        }

        case clang::TemplateName::SubstTemplateTemplateParm:
            tempIdent = tm.getIdentifierForTemplateTemplateParm(
                    Name.getAsSubstTemplateTemplateParm()->getParameter());
            break;

        case clang::TemplateName::DependentTemplate:
            tempIdent = fromIdentifier(
                    Name.getAsDependentTemplateName()->getIdentifier());
            break;

        default:
            assert(false && "Unsupported template name kind");
            return nullptr;
    };

    if (ArgBegin)
    {
        auto ti = new cpp::TemplateInstance(loc, tempIdent);
        ti->tiargs = fromTemplateArguments(ArgBegin, ArgEnd,
                                        Name.getAsTemplateDecl()->getTemplateParameters());

        return new TypeInstance(loc, ti);
    }
    else
        return new TypeIdentifier(loc, tempIdent);
}

Type* TypeMapper::FromType::fromTypeTemplateSpecialization(const clang::TemplateSpecializationType* T)
{
    auto tqual = fromTemplateName(T->getTemplateName(),
                            T->begin(), T->end());

    if (!tqual)
        return nullptr; // discard types leading to recursive template expansion

    auto setInst = [&] (TemplateInstUnion Inst) {
        if (isDependent)
            return;

        auto o = tqual->idents.empty() ? typeQualifiedRoot(tqual) : tqual->idents.back();

        // NOTE: o may be an identifier if this is an « injected scope name »
        if (o && o->dyncast() == DYNCAST_DSYMBOL)
        {
            auto s = static_cast<Dsymbol*>(o);
            assert(s->isTemplateInstance() && isCPP(s));

            auto ti = static_cast<cpp::TemplateInstance*>(s);

            ti->Inst = Inst;
        }
    };

    if (T->isTypeAlias())
        setInst(T);
    else if (T->isSugared())
    {
        auto RT = T->getAs<clang::RecordType>();

        if (RT)
        {
            if (auto subst = tm.trySubstitute(RT->getDecl())) // HACK for correctTiargs
                return subst;

            auto RD = RT->getDecl();
            if (tm.isInjectedClassName(RD))
                return fromInjectedClassName(loc);
        }

        // NOTE: To reduce DMD -> Clang translations to a minimum we don't instantiate ourselves whenever possible, i.e when
        // the template instance is already declared or defined in the PCH. If it's only declared, we tell Sema to complete its instantiation.
        if (RT)
            setInst(RT->getDecl());
    }

    return tqual;
}

Identifier *TypeMapper::getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmDecl *D,
                                const clang::TemplateTypeParmType *T)
{
    unsigned Depth, Index;

    if (D) {
        if (auto Id = D->getIdentifier())
            return fromIdentifier(Id);
        Depth = D->getDepth();
        Index = D->getIndex();
    } else {
        assert(T);
        Depth = T->getDepth();
        Index = T->getIndex();
    }

    // NOTE: Most of the time the identifier does exist in the TemplateTypeParmDecl even if TemplateTypeParmType::getIdentifier() returns null
    // Parameter without identifier should only ever happen in template param decl mapping
    if (opts::cppVerboseDiags)
        ::warning(D ? fromLoc(D->getLocation()) : Loc(),
                        "Generating identifier for anonymous C++ type template parameter");

    std::string str;
    llvm::raw_string_ostream OS(str);
    OS << "type_parameter_" << Depth << '_' << Index;

    return Identifier::idPool(OS.str().c_str());
}

Identifier *TypeMapper::getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D)
{
    // TODO: merge with others?

    if (auto Id = D->getIdentifier())
        return fromIdentifier(Id);

    // This should only ever happen in template param decl mapping
    if (opts::cppVerboseDiags)
        ::warning(fromLoc(D->getLocation()), "Generating identifier for anonymous C++ template template parameter");

    std::string str;
    llvm::raw_string_ostream OS(str);
    OS << "template_parameter_" << D->getDepth() << '_' << D->getIndex();

    return Identifier::idPool(OS.str().c_str());
}

unsigned getTemplateParmIndex(const clang::NamedDecl *ParmDecl)
{
    if (auto TTPD = dyn_cast<clang::TemplateTypeParmDecl>(ParmDecl))
        return TTPD->getIndex();
    else if (auto NTTPD = dyn_cast<clang::NonTypeTemplateParmDecl>(ParmDecl))
        return NTTPD->getIndex();
    else if (auto TTempPD = dyn_cast<clang::TemplateTemplateParmDecl>(ParmDecl))
        return TTempPD->getIndex();
    else
        assert(false && "Unrecognized template parameter decl");
    return 0;
}

unsigned getTemplateParmDepth(const clang::NamedDecl *ParmDecl)
{
    if (auto TTPD = dyn_cast<clang::TemplateTypeParmDecl>(ParmDecl))
        return TTPD->getDepth();
    else if (auto NTTPD = dyn_cast<clang::NonTypeTemplateParmDecl>(ParmDecl))
        return NTTPD->getDepth();
    else if (auto TTempPD = dyn_cast<clang::TemplateTemplateParmDecl>(ParmDecl))
        return TTempPD->getDepth();
    else
        assert(false && "Unrecognized template parameter decl");
    return 0;
}

const clang::TemplateTypeParmDecl *TypeMapper::FromType::getOriginalTempTypeParmDecl(const clang::TemplateTypeParmType *T)
{
    auto ParmDecl = T->getDecl();
    if (!ParmDecl)
        return nullptr; // may happen for partial template specs arguments (meant for the master template params)

    auto ParmCtx = cast<clang::Decl>(ParmDecl->getDeclContext())->getCanonicalDecl();

    for (auto I = tm.TempParamScope.rbegin(), E = tm.TempParamScope.rend();
                I != E; I++)
    {
        for (auto& ScopeParam: (*I)->asArray())
        {
            auto ScopeParamCtx = cast<clang::Decl>(ScopeParam->getDeclContext())->getCanonicalDecl();
            if (ScopeChecker(ParmCtx, true)(ScopeParamCtx) &&
                        getTemplateParmIndex(ParmDecl) == getTemplateParmIndex(ScopeParam))
                return cast<clang::TemplateTypeParmDecl>(ScopeParam);
        }
    }

    return nullptr;
}

Type* TypeMapper::FromType::fromTypeTemplateTypeParm(const clang::TemplateTypeParmType* T,
                        const clang::TemplateTypeParmDecl *OrigDecl)
{
    if (!OrigDecl)
        OrigDecl = getOriginalTempTypeParmDecl(T);

    if(!OrigDecl)
        assert(T->isDependentType());

    auto D = OrigDecl ? OrigDecl : T->getDecl();
    auto ident = tm.getIdentifierForTemplateTypeParm(D, T);
    return new TypeIdentifier(loc, ident);
}

Type* TypeMapper::FromType::fromTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType* T)
{
    // NOTE: it's necessary to "revert" resolved symbol names of C++ template instantiations by Sema to the parameter name because D severes the link between the template instance scope and its members, and the only links that remain are the AliasDeclarations created by TemplateInstance::declareParameters

    // One exception is when the type managed to escape the template declaration, e.g with decltype(). In this fairly rare case T has to be desugared.
    auto OrigParm = getOriginalTempTypeParmDecl(T->getReplacedParameter());

    if (!OrigParm)
    {
//         // If the substitued argument comes from decltype(some function template call), then the fragile link that makes perfect C++ template mapping possible (type sugar) is broken.
//         // Clang has lost the template instance at this point, so first we get it back from the decltype expr.
//         if (auto CE = llvm::dyn_cast_or_null<clang::CallExpr>(TypeOfExpr))
//             if (auto Callee = CE->getDirectCallee())
//                 if (Callee->isTemplateInstantiation())
//                 {
//                     // Secondly the substitued template argument is the one of the function template arg, but if the argument was deduced from the call args then type sugar is lost forever (either typedef, subst template arg, and maybe other kinds of sugar), this is where it gets complicated.
//                     // The laziest and "should work in most cases" solution is to use DMD's own overloading and template argument deduction from the original decltype expression.
//
//                     ExprMapper em(tm);
//
//                     auto e = em.fromExpression(TypeOfExpr);
//                     auto loc = fromLoc(TypeOfExpr->getExprLoc());
//
//                     return new TypeTypeof(loc, e);
//
//                     // NOTE: Sugar can't be preserved because Clang could have call arg with typedef types where the typedef decl isn't usable to get back the template arg sugar, e.g template<T> void Func(T *a); decltype(Func(someTypedef));
//                     // Another possible solution would be to make a deduction listener that records the deduction actions to apply them on the call arg types, but it's much more complex.
//                 }

        return fromType(T->getReplacementType());
    }

    return fromTypeTemplateTypeParm(T->getReplacedParameter(), OrigParm);
}

Type* TypeMapper::FromType::fromTypeInjectedClassName(const clang::InjectedClassNameType* T) // e.g in template <...> class A { A &next; } next has an injected class name type
{
    // A simple identifier is preferrable when possible, as mapping template arguments requires switching to a different parameter scope
    return new TypeIdentifier(loc, getIdentifier(T->getDecl()));

//     return typeSubstOrQualifiedFor(T->getDecl());
        // NOTE: this will return typeof(this) if we aren't in a nested class, but if we are the name of the record is (without template arguments)
}

TypeQualified *TypeMapper::FromType::fromNestedNameSpecifierImpl(const clang::NestedNameSpecifier *NNS)
{
    TypeQualified *result = nullptr;

    switch (NNS->getKind())
    {
        case clang::NestedNameSpecifier::Identifier:
        {
            auto ident = fromIdentifier(NNS->getAsIdentifier());
            result = new TypeIdentifier(loc, ident);
            break;
        }

        case clang::NestedNameSpecifier::TypeSpec:
        case clang::NestedNameSpecifier::TypeSpecWithTemplate:
        {
            auto t = fromTypeUnqual(NNS->getAsType());
            if (!t)
                return nullptr;
            assert(t->ty == Tinstance || t->ty == Tident || t->ty == Ttypeof);
            result = (TypeQualified*) t;
            break;
        }

        case clang::NestedNameSpecifier::Namespace:
        case clang::NestedNameSpecifier::NamespaceAlias:
        case clang::NestedNameSpecifier::Global:
            return nullptr;  // not dependent, no derived <> base decl context issue so building a TypeQualified after the NNS is unnecessary

        default:
            assert(false && "Unsupported nested name specifier kind");
    }

    return result;
}

TypeQualified* TypeMapper::FromType::fromNestedNameSpecifier(const clang::NestedNameSpecifier* NNS)
{
    if (auto Prefix = NNS->getPrefix())
        if (auto tqual = fromNestedNameSpecifier(Prefix))
            return FromType(tm, loc, tqual).fromNestedNameSpecifierImpl(NNS);

    return fromNestedNameSpecifierImpl(NNS);
}

// NOTE: Dependent***Type are not mandatory to get templates working because the instantiation is done by Sema
// and then DMD simply maps the resulting class or function specialization, so we could return TypeNull and it would still work.
// Still good for reflection.
Type* TypeMapper::FromType::fromTypeDependentName(const clang::DependentNameType* T)
{
    TypeQualified *tqual = nullptr;

    if (auto NNS = T->getQualifier())
        tqual = fromNestedNameSpecifier(NNS);

    auto ident = fromIdentifier(T->getIdentifier());
    if (!tqual)
        tqual = new TypeIdentifier(loc, ident);
    else
        tqual->addIdent(ident);

    return tqual;
}

Type* TypeMapper::FromType::fromTypeDependentTemplateSpecialization(const clang::DependentTemplateSpecializationType* T)
{
    TypeQualified *tqual = nullptr;

    if (auto NNS = T->getQualifier())
        tqual = fromNestedNameSpecifier(NNS);

    auto ident = fromIdentifier(T->getIdentifier());
    auto tiargs = fromTemplateArguments(T->begin(), T->end());

    auto tempinst = new ::TemplateInstance(loc, ident);
    tempinst->tiargs = tiargs;

    if (!tqual)
        tqual = new TypeInstance(loc, tempinst);
    else
        tqual->addInst(tempinst);

    return tqual;
}

template <typename _Type>
Type *TypeMapper::FromType::fromTypeOfExpr(const _Type *T)
{
    if (T->isSugared())  // TODO: remove this for reflection?
    {
        FromType underlying(tm, loc);
        underlying.TypeOfExpr = T->getUnderlyingExpr(); // needed for SubstTemplateTypeParm

        return underlying(T->desugar());
    }

    auto exp = ExprMapper(tm).fromExpression(T->getUnderlyingExpr());
    assert(exp);
    return new TypeTypeof(loc, exp);
}

Type* TypeMapper::FromType::fromTypeTypeOfExpr(const clang::TypeOfExprType* T)
{
    return fromTypeOfExpr(T);
}

Type* TypeMapper::FromType::fromTypeDecltype(const clang::DecltypeType* T)
{
    return fromTypeOfExpr(T);
}

Type* TypeMapper::FromType::fromTypePackExpansion(const clang::PackExpansionType* T)
{
    return fromType(T->getPattern());
}

// This is to check whether template arguments have to be omitted
// There may be a more elegant way but for now that'll do
bool TypeMapper::isInjectedClassName(const clang::Decl *D)
{
    if(!CXXScope.empty())
    {
        auto ScopeDecl = CXXScope.top()->getCanonicalDecl();

        if (ScopeDecl == D->getCanonicalDecl())
            return true;
    }

    return false;
}

bool TypeMapper::isInjectedScopeName(const clang::Decl *D)
{
    decltype(TypeMapper::CXXScope) ScopeStack(CXXScope);
    while(!ScopeStack.empty())
    {
        auto ScopeDecl = ScopeStack.top()->getCanonicalDecl();
        
        if (ScopeDecl == D->getCanonicalDecl())
            return true;

        ScopeStack.pop();
    }

    return false;
}

TypeFunction *TypeMapper::FromType::fromTypeFunction(const clang::FunctionProtoType* T,
        const clang::FunctionDecl *FD)
{
    if (T->isVolatile())
        tm.volatileNumber++;

    auto& Context = calypso.getASTContext();
    auto& S = calypso.getSema();
    auto& Diags = calypso.getDiagnostics();

    auto params = new Parameters;
    params->reserve(T->getNumParams());

    decltype(FD->param_begin()) PI;
    if (FD)
        PI = FD->param_begin();

    for (auto I = T->param_type_begin(), E = T->param_type_end();
                I != E; I++)
    {
        StorageClass stc = STCundefined;
        auto at = tm.fromType(*I, loc);
        Identifier *ident = nullptr;
        Expression *defaultArg = nullptr;

        if (!at)
            return nullptr;

        // Turn a TypeReference into « ref nextOf() » as early as possible as this helps function resolving
        if (at->ty == Treference)
        {
            stc |= STCscope | STCref;
            at = at->nextOf();
        }

        if (FD)
        {
            ident = getIdentifierOrNull(*PI);

            if ((*PI)->hasDefaultArg())
            {
                clang::Expr *DefaultArgExpr;
                bool isUninstantiated = (*PI)->hasUninstantiatedDefaultArg();

                if (isUninstantiated &&
                        (FD->getInstantiatedFromMemberFunction() || FD->isTemplateInstantiation()))
                {
                    DefaultArgExpr = S.BuildCXXDefaultArgExpr(FD->getPointOfInstantiation(),
                                                              const_cast<clang::FunctionDecl*>(FD), *PI).get();
                }
                else
                    DefaultArgExpr = isUninstantiated ?
                                (*PI)->getUninstantiatedDefaultArg() : (*PI)->getDefaultArg();

                if (DefaultArgExpr) // might be null if BuildCXXDefaultArgExpr returned ExprError
                    defaultArg = ExprMapper(tm).fromExpression(DefaultArgExpr);

                if (Diags.hasErrorOccurred())
                    Diags.Reset();
            }

            PI++;
        }

        params->push(new Parameter(stc, at, ident, defaultArg));
    }

    StorageClass stc = STCundefined;
    if (T->isConst())
        stc |= STCconst;

    Type *rt;
    if (isa<clang::AutoType>(T->getReturnType())) {
        stc |= STCauto;
        rt = nullptr;
    } else {
        rt = FromType(tm, loc)(T->getReturnType());
        if (!rt)
            return nullptr;

        if (rt->ty == Treference)
        {
            stc |= STCref;
            rt = rt->nextOf();
        }
    }

    if (T->isNothrow(Context, false))
        stc |= STCnothrow;

    LINK linkage = (FD && FD->isExternC()) ? LINKc : LINKcpp;
        // TODO: inferring the linkage for overriding methods would be nice

    auto tf = new TypeFunction(params, rt, 0, linkage, stc);
    tf = static_cast<TypeFunction*>(tf->addSTC(stc));
    return tf;
}

static clang::Module *GetClangModuleForDecl(const clang::Decl* D)
{
#ifdef USE_CLANG_MODULES
    auto& SrcMgr = calypso.getSourceManager();
    auto MMap = calypso.pch.MMap;

    if (!MMap)
        return nullptr;

    auto Loc = D->getLocStart();
    auto DLoc = SrcMgr.getFileLoc(Loc);
    if (!DLoc.isFileID())
        return nullptr;

    auto FID = SrcMgr.getFileID(DLoc);
    auto Header = SrcMgr.getFileEntryForID(FID);
    if (!Header)
    {
        assert(D->isImplicit()); // maybe too narrow..
        return nullptr;
    }

    auto KH = MMap->findModuleForHeader(Header);
    if (!KH)
        return nullptr;

    return KH.getModule();
#else
    return nullptr;
#endif
}

// In D if a class is inheriting from another module's class, then its own module has to import the base class' module.
// So we need to populate the beginning of our virtual module with imports for derived classes.
::Import *TypeMapper::AddImplicitImportForDecl(Loc loc, const clang::NamedDecl *D, bool fake)
{
    if (!fake && !addImplicitDecls)
        return nullptr;

    auto Key = GetImplicitImportKeyForDecl(D);

    if (mod && Key == mod->rootKey)
        return nullptr; // do not import self

    if (!fake && implicitImports[Key])
        return nullptr; // already imported

    Identifier *importAliasid = nullptr;
    if (mod && isa<clang::TagDecl>(Key.first) &&
            getDeclContextNamedOrTU(Key.first)->isTranslationUnit())
    {
        // Special check for C tags which may have the same name as functions
        // When this does happens, rename the import
        // NOTE: is this really specific to tags and functions?
        auto TD = cast<clang::TagDecl>(Key.first);
        auto TU = TD->getTranslationUnitDecl();

        auto R = TU->lookup(TD->getDeclName());
        for (auto Match: R)
        {
            if (Match->getCanonicalDecl() == Key.first)
                continue;
            if (GetImplicitImportKeyForDecl(Match) != mod->rootKey)
                continue; // the matching decl is part of another module, no conflict

            { auto importIdent = getIdentifier(TD);
            llvm::SmallString<48> s(u8"§"); // non-ASCII but pretty
            s += llvm::StringRef(importIdent->string, importIdent->len);
            importAliasid = Identifier::idPool(s.c_str()); }

            assert(!Key.second);
            break;
        }
    }

    ::Import *im;
    if (Key.second)
        im = BuildImplicitImport(loc, Key.first, Key.second, importAliasid);
    else
        im = BuildImplicitImport(loc, Key.first, importAliasid);

    if (!fake)
    {
        implicitImports[Key] = im;
        mod->members->shift(im);
    }

    return im;
}

Module::RootKey TypeMapper::GetImplicitImportKeyForDecl(const clang::NamedDecl* D)
{
    D = cast<clang::NamedDecl>(getCanonicalDecl(D));

    if (D->getFriendObjectKind() != clang::Decl::FOK_None && D->isOutOfLine() &&
            isa<clang::FunctionDecl>(D))
        return GetImplicitImportKeyForDecl(  // friend declarations which aren't redeclared in the semantic declctx are part of the record module
                cast<clang::NamedDecl>(D->getLexicalDeclContext()));

    auto TopMost = GetNonNestedContext(D);
    bool IsNamespaceOrTU = isa<clang::TranslationUnitDecl>(TopMost) ||
                    isa<clang::NamespaceDecl>(TopMost);

    auto Func = dyn_cast<clang::FunctionDecl>(D);
    if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
        Func = FuncTemp->getTemplatedDecl();
    if (IsNamespaceOrTU && Func && Func->isOverloadedOperator())
        if (auto Tag = isOverloadedOperatorWithTagOperand(D)) // non-member operators are part of the record module
            return GetImplicitImportKeyForDecl(Tag);

    if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(TopMost))
        return GetImplicitImportKeyForDecl(Spec->getSpecializedTemplate());

    Module::RootKey Key;
    Key.first = TopMost->getCanonicalDecl();
    if (IsNamespaceOrTU)
        Key.second = GetClangModuleForDecl(D); // see if there's a Clang module which contains the decl
    return Key;
}

// Remove sugar other than aliases
clang::QualType withoutNonAliasSugar(clang::QualType Ty)
{
    auto& Context = calypso.getASTContext();

    auto OneStepDesugar = Ty.getSingleStepDesugaredType(Context);
    while ((isa<clang::ElaboratedType>(*Ty) ||
                isa<clang::ParenType>(*Ty) ||
                isa<clang::AdjustedType>(*Ty)) &&
            OneStepDesugar.getTypePtr() != Ty.getTypePtr())
    {
        Ty = OneStepDesugar;
        OneStepDesugar = Ty.getSingleStepDesugaredType(Context);
    }

    return Ty;
}

// typedef class/struct/enum { ...anon record... } SymbolName
// are special cases, they're mapped to D aggregates instead of aliases
const clang::TagDecl *isAnonTagTypedef(const clang::TypedefNameDecl* D)
{
    auto Ty = withoutNonAliasSugar(D->getUnderlyingType());

    if (auto TagTy = dyn_cast<clang::TagType>(Ty.getTypePtr()))
    {
        auto Tag = TagTy->getDecl();

        if (Tag->getTypedefNameForAnonDecl())
            return Tag;
    }

    return nullptr;
}

// Returns true e.g typedef union pthread_attr_t pthread_attr_t
// The typedef need to be discarded, VisitTypedefType needs to map the underlying type
bool isSameNameTagTypedef(const clang::TypedefNameDecl* D)
{
    auto Ty = D->getUnderlyingType();

    if (auto TagTy = Ty->getAs<clang::TagType>())
    {
        auto Tag = TagTy->getDecl();

        auto Parent = cast<clang::Decl>(getDeclContextNamedOrTU(D));
        auto TagParent = cast<clang::Decl>(getDeclContextNamedOrTU(Tag));

        if (Tag->getName() == D->getName() &&
                TagParent->getCanonicalDecl() == Parent->getCanonicalDecl())
            return true;
    }

    return false;
}

// Returns the topmost parent tagdecl, or the bottom-most namespace or the TU
const clang::Decl *GetNonNestedContext(const clang::Decl *D)
{
    if (isa<clang::TranslationUnitDecl>(D) ||
                isa<clang::NamespaceDecl>(D))
        return D;

    if (auto ClassTemp = dyn_cast<clang::ClassTemplateDecl>(D))
        return GetNonNestedContext(ClassTemp->getTemplatedDecl());

    if (auto Typedef = dyn_cast<clang::TypedefNameDecl>(D))
        if (auto AnonTag = isAnonTagTypedef(Typedef))
            D = AnonTag;

    if (auto TagDC = dyn_cast<clang::TagDecl>(D->getDeclContext()))
        return GetNonNestedContext(TagDC);
    if (auto FuncDC = dyn_cast<clang::FunctionDecl>(D->getDeclContext()))
        return GetNonNestedContext(FuncDC);

    auto Tag = dyn_cast<clang::TagDecl>(D);
    if (Tag && getIdentifierOrNull(Tag))
        return D;

    return GetNonNestedContext(cast<clang::Decl>(
                        getDeclContextNamedOrTU(D)));
}

static Identifier *BuildImplicitImportInternal(const clang::DeclContext *DC,
                                               Loc loc, Identifiers *sPackages)
{
    if (DC->isTranslationUnit()) return nullptr;
    assert(!DC->isFunctionOrMethod() && "Building import for a decl nested inside a func?");

    if (auto sModule = BuildImplicitImportInternal(
                getDeclContextNamedOrTU(cast<clang::Decl>(DC)), loc, sPackages))
        return sModule;

    if (auto NS = dyn_cast<clang::NamespaceDecl>(DC))
    {
        if (NS->isAnonymousNamespace())
            error(loc, "Cannot import symbols from anonymous namespaces");

        if (!NS->isInline())
            sPackages->push(getIdentifier(NS));

        return nullptr;
    }
    else if (isa<clang::TagDecl>(DC))
        return getIdentifier(cast<clang::NamedDecl>(DC));

    llvm_unreachable("Unhandled case");
}

::Import *TypeMapper::BuildImplicitImport(Loc loc, const clang::Decl *D, Identifier *aliasid)
{
    auto DC = cast<clang::DeclContext>(D);

    auto sPackages = new Identifiers;
    auto sModule = BuildImplicitImportInternal(DC, loc, sPackages);

    if (!sModule)
    {
        if (isa<clang::ClassTemplateDecl>(D))
            sModule = getIdentifier(cast<clang::NamedDecl>(D));
        else
            // D is neither a tag nor a class template, we need to import the namespace's functions and vars
            sModule = Identifier::idPool("_");
    }

    return new cpp::Import(loc, sPackages, sModule, aliasid, 1);
}

::Import *TypeMapper::BuildImplicitImport(Loc loc, const clang::Decl *D, const clang::Module *Mod,
                                          Identifier *aliasid)
{
    auto sPackages = new Identifiers;

    if (!isa<clang::TranslationUnitDecl>(D))
    {
        auto loc = fromLoc(D->getLocation());
        auto DC = cast<clang::DeclContext>(D);

        BuildImplicitImportInternal(DC, loc, sPackages);
    }

    auto insertIndex = sPackages->dim;
    auto sModule = Identifier::idPool(Mod->Name.c_str());

    auto M = Mod->Parent;
    while (M)
    {
        sPackages->insert(insertIndex, Identifier::idPool(M->Name.c_str()));
        M = M->Parent;
    }

    return new cpp::Import(loc, sPackages, sModule, aliasid, 1);
}

void TypeMapper::pushTempParamList(const clang::Decl *D)
{
    const clang::TemplateParameterList *TPL = nullptr;

    if (auto ClassTemp = dyn_cast<clang::ClassTemplateDecl>(D))
        TPL = getDefinition(ClassTemp)->getTemplateParameters();
    else if (auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(D))
        TPL = cast<clang::ClassTemplatePartialSpecializationDecl>(
                        getDefinition(Partial))->getTemplateParameters();
    else if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
        TPL = getDefinition(FuncTemp)->getTemplateParameters();

    if (TPL)
    {
        TempParamScope.push_back(TPL);
        return;
    }

    if (auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
    {
        if (!ClassSpec->isExplicitSpecialization())
            pushTempParamList(getSpecializedDeclOrExplicit(ClassSpec));
    }
    else if (auto Func = dyn_cast<clang::FunctionDecl>(D))
    {
        if (auto PrimTemp = Func->getPrimaryTemplate())
            pushTempParamList(PrimTemp);
    }
}

// NOTE: unlike Sema::getTemplateInstantiationArgs in D we need to take all the template parameters,
// not just the ones needed to instantiate the template in C++
void TypeMapper::rebuildScope(const clang::Decl *RightMost)
{
    assert(CXXScope.empty() && TempParamScope.empty());

    // Recreate the scope, esp. important for nested template instances
    std::function<void(const clang::Decl *)> build =
                    [&] (const clang::Decl *D)
    {
        if (isa<clang::TranslationUnitDecl>(D))
            return;

        build(cast<clang::Decl>(D->getDeclContext()));

        pushTempParamList(D);

        if (isa<clang::CXXRecordDecl>(D) || isa<clang::FunctionDecl>(D))
            CXXScope.push(D);
    };

    build(RightMost);
}

/***** DMD -> Clang types *****/

clang::QualType TypeMapper::toType(Loc loc, Type* t, Scope *sc, StorageClass stc)
{
    auto& Context = calypso.getASTContext();

    if (stc & STCref)
    {
        t = new TypeReference(t);
        stc &= ~STCref;
    }

    if (t->isConst() || t->isImmutable())
    {
        t = t->nullAttributes();
        t->mod &= ~(MODconst|MODimmutable);
        return toType(loc, t, sc, stc).withConst();
    }

    t = t->merge2();

    if (auto builtin = calypso.builtinTypes.toClang[t])
        return clang::QualType(builtin, 0);

    switch (t->ty)
    {
        case Tstruct:
        {
            // Special treatment of __cpp_member_funcptr!(T, Cls)
            auto sd = static_cast<TypeStruct*>(t)->sym;
            if (sd->ident == Identifier::idPool("__cpp_member_funcptr"))
            {
                auto ti = sd->toParent()->isTemplateInstance();
                assert(ti);
                auto ft = isType((*ti->tiargs)[0]);
                auto tagg = isType((*ti->tiargs)[1]);
                assert(ft && ft->ty == Tfunction &&
                        tagg && (tagg->ty == Tstruct || tagg->ty == Tclass) && isCPP(getAggregateSym(tagg)));

                auto FT = toType(loc, ft, sc);
                auto ClassParent = toType(loc, tagg, sc);

                return Context.getMemberPointerType(FT, ClassParent.getTypePtr());
            }
        }
        case Tclass:
        {
            return Context.getRecordType(getRecordDecl(t));
        }
        case Tenum:
        {
            auto ed = static_cast<TypeEnum*>(t)->sym;
            assert(isCPP(ed));
            auto ED = static_cast<cpp::EnumDeclaration*>(ed)->ED;

            return Context.getEnumType(ED);
        }
        case Tident:
        case Tinstance:
        {
            t = t->semantic(loc, sc);
            return toType(loc, t, sc, stc);
        }
        case Tpointer:
        case Treference:
        {
            auto Pointee = toType(loc, t->nextOf(), sc);

            if (t->ty == Tpointer)
                return Context.getPointerType(Pointee);
            else {
                auto tref = static_cast<TypeReference*>(t);
                return tref->isRvalueRef ? Context.getRValueReferenceType(Pointee)
                            : Context.getLValueReferenceType(Pointee);
            }
        }
        case Tfunction:
        {
            auto tf = static_cast<TypeFunction*>(t);

            auto ResultTy = toType(loc, tf->next, sc, tf->isref ? STCref : 0);

            llvm::SmallVector<clang::QualType, 4> Args;
            for (auto p: *tf->parameters)
                Args.push_back(toType(loc, p->type, sc, p->storageClass));

            clang::FunctionProtoType::ExtProtoInfo EPI;
            return Context.getFunctionType(ResultTy, Args, EPI);
        }
        // TODO arrays
    }

    llvm::llvm_unreachable_internal("Unhandled D -> Clang type conversion");
}

/***** *****/

TypeMapper::TypeMapper(cpp::Module* mod)
    : mod(mod)
{
}

// NOTE: doesn't return null if the template isn't defined. What we really want is some sort of canonical declaration to refer to for template parameter names.
template <typename RedeclTempDecl>
 const RedeclTempDecl *getRTDDefinition(const RedeclTempDecl *D, bool memberTemplate = true)
{
    for (auto RI: D->redecls()) // find the definition if any
    {
        auto I = cast<RedeclTempDecl>(RI);
        if (I->isThisDeclarationADefinition())
            return I;
    }

    // This is more heuristical than anything else.. I'm not sure yet why templates inside
    // specializations (e.g std::allocator::rebind) do not get defined.
    if (memberTemplate)
        if (auto MemberTemp = const_cast<RedeclTempDecl*>(D)->getInstantiatedFromMemberTemplate())
            if (auto MemberDef = getRTDDefinition(MemberTemp))
                return MemberDef;

    return cast<RedeclTempDecl>(getCanonicalDecl(D));
}

const clang::ClassTemplateDecl *getDefinition(const clang::ClassTemplateDecl *D)
{
    return getRTDDefinition(D);
}

const clang::FunctionTemplateDecl *getDefinition(const clang::FunctionTemplateDecl *D)
{
    return getRTDDefinition(D);
}

const clang::RedeclarableTemplateDecl* getDefinition(const clang::RedeclarableTemplateDecl* D,
                                                     bool lookIntoMemberTemplate)
{
    if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
        return getRTDDefinition(CTD, lookIntoMemberTemplate);
    else if (auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D))
        return getRTDDefinition(FTD, lookIntoMemberTemplate);
    else if (auto VTD = dyn_cast<clang::VarTemplateDecl>(D))
        return getRTDDefinition(VTD, lookIntoMemberTemplate);
    return D;
}

const clang::ClassTemplateSpecializationDecl *getDefinition(const clang::ClassTemplateSpecializationDecl *D)
{
    if (auto Definition = D->getDefinition())
        return cast<clang::ClassTemplateSpecializationDecl>(Definition);

    if (auto Partial = dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(D))
        if (auto MemberInst = const_cast<clang::ClassTemplatePartialSpecializationDecl*>(Partial)->getInstantiatedFromMember()) // not the same method name..
            if (auto MemberDef = getDefinition(MemberInst))
                return MemberDef;

    return D;
}

const clang::DeclContext *getDeclContextNonLinkSpec(const clang::Decl *D)
{
    auto DC = D->getDeclContext();

    while (isa<clang::LinkageSpecDecl>(DC))
        DC = DC->getParent();

    return DC;
}

const clang::DeclContext *getDeclContextNamedOrTU(const clang::Decl *D)
{
    auto DC = getDeclContextNonLinkSpec(D);
    auto NamedDC = dyn_cast<clang::NamedDecl>(DC);
    
    if (NamedDC && NamedDC->getDeclName().isEmpty())
    {
        if (auto Tag = llvm::dyn_cast<clang::TagDecl>(DC))
            if (Tag->getTypedefNameForAnonDecl())
                return DC;

        return getDeclContextNamedOrTU(NamedDC);
    }

    assert(NamedDC || isa<clang::TranslationUnitDecl>(DC));
    return DC;
}

// Helper function for GDB
const clang::Decl *getParent(const clang::Decl *D)
{
    return cast<clang::Decl>(D->getDeclContext());
}

const clang::Decl *getSpecializedDeclOrExplicit(const clang::Decl *Spec)
{
    if (auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(Spec))
    {
        if (ClassSpec->isExplicitSpecialization())
            return Spec;

        auto U = ClassSpec->getSpecializedTemplateOrPartial();

        if (U.is<clang::ClassTemplateDecl*>())
            return U.get<clang::ClassTemplateDecl*>();
        else
            return U.get<clang::ClassTemplatePartialSpecializationDecl*>();
    }
    else if (auto FuncSpec = dyn_cast<clang::FunctionDecl>(Spec))
    {
        assert(FuncSpec->getPrimaryTemplate());

        if (FuncSpec->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization)
            return Spec;

        return FuncSpec->getPrimaryTemplate();
    }

    llvm_unreachable("Not a template spec?");
}

}
