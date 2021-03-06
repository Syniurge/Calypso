// Contributed by Elie Morisse, same license DMD uses

#include "cpp/calypso.h"
#include "cpp/cppaggregate.h"
#include "cpp/cppdeclaration.h"
#include "cpp/cppexpression.h"
#include "cpp/cppimport.h"
#include "cpp/cppmodule.h"
#include "cpp/cpptemplate.h"
#include "cpp/ddmdstructor.h"
#include "cpp/ddmdvisitor.h"
#include "driver/cl_options.h"
#include "errors.h"
#include "id.h"
#include "identifier.h"
#include "module.h"
#include "template.h"

#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/Type.h"
#include "clang/Sema/Sema.h"

namespace cpp
{

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

/***** Calypso-specific types *****/

// Internal Calypso types essential for template arguments matching.
// D's const being transitive and since Calypso only needs logical const internally, keeping it outside of DMD seemed like the better idea.

class TypePointer : public ::TypePointer
{
public:
    CALYPSO_LANGPLUGIN

    TypePointer(Type *t) { construct_TypePointer(this, t); }

    Type *syntaxCopy(/*Type *o = nullptr*/) override
    {
        Type* t = next->syntaxCopy();
        if (t == next)
            t = this;
        else
        {
            t = new TypePointer(t);
            t->mod = mod;
        }
        return t;
    }

    bool isTransitive() override { return false; }
//     bool isMergeable() override { return false; }
    unsigned short sizeType() const override { return sizeof(*this); }

    virtual bool isIncompleteArray() const { return false; }

    void accept(Visitor *v) override
    {
        auto v_ti = v->_typeid();

        if (v_ti == TI_Mangler) { // mangle
            auto buf = static_cast<Mangler*>(v)->buf;
            buf->writeUTF8(2102); // U+2102 = ℂ
            v->visit(this);
        } else
            v->visit(this);
    }
};

// C array with unspecified size e.g int a[]
class TypeIncompleteArray : public TypePointer
{
public:
    TypeIncompleteArray(Type *t) : TypePointer(t) {}

    Type *syntaxCopy() override
    {
        Type* t = next->syntaxCopy();
        if (t == next)
            t = this;
        else
        {
            t = new TypeIncompleteArray(t);
            t->mod = mod;
        }
        return t;
    }

    unsigned short sizeType() const override { return sizeof(*this); }
    bool isIncompleteArray() const override { return true; }

    void accept(Visitor *v) override
    {
        auto v_ti = v->_typeid();

        if (v_ti == TI_Mangler) { // mangle
            auto buf = static_cast<Mangler*>(v)->buf;

            buf->writeUTF8(2102); // U+2102 = ℂ
            v->visit(this);
            buf->writeByte('#');
            buf->writeByte('A');
        } else
            v->visit(this);
    }
};

class TypeReference : public ::TypeReference
{
public:
    CALYPSO_LANGPLUGIN

    bool isRvalueRef;

    TypeReference(Type *t, bool isRvalueRef = false)
    {
        construct_TypeReference(this, t);
        this->isRvalueRef = isRvalueRef;
    }

    Type *syntaxCopy(/*Type *o = nullptr*/) override
    {
        Type* t = next->syntaxCopy();
        if (t == next)
            t = this;
        else
        {
            t = new TypeReference(t);
            t->mod = mod;
        }
        return t;
    }

    bool isRvalRef() const override { return isRvalueRef; }
    bool isTransitive() override { return false; }
//     bool isMergeable() override { return false; }
    unsigned short sizeType() const override { return sizeof(*this); }

    void accept(Visitor *v) override
    {
        auto v_ti = v->_typeid();

        if (v_ti == TI_Mangler) { // mangle
            auto buf = static_cast<Mangler*>(v)->buf;
            buf->writeUTF8(2102); // U+2102 = ℂ
            if (isRvalueRef) {
                v->visit(static_cast<Type*>(this)); // only 'R'
                buf->writeByte('#');
            }
            v->visit(this);
        } else
            v->visit(this);
    }
};

//  There are a few D builtin types that are mapped to several C++ ones, such as wchar_t/dchar <=> wchar_t. Even though they're the same, we have to differentiate them (e.g char_traits<wchar_t>char_traits<char32>) or else two template instances might have different tempdecl while their aggregate member get the same deco
class TypeBasic : public ::TypeBasic
{
public:
    CALYPSO_LANGPLUGIN

    const clang::BuiltinType *T;
    unsigned idx;

    static unsigned nextIdx[TMAX];

    TypeBasic(TY ty, const clang::BuiltinType *T = nullptr)
    {
        this->T = T; // NOTE: the order matters here, the D ctor calls merge() which gets the type mangled so T has to be set
        this->idx = nextIdx[ty]++;
        construct_TypeBasic(this, ty);
    }

    unsigned short sizeType() const override
    {
        return sizeof(*this);
    }

    void accept(Visitor *v) override
    {
        auto v_ti = v->_typeid();

        if (v_ti == TI_Mangler) { // mangle
            auto buf = static_cast<Mangler*>(v)->buf;

            v->visit(this);
            buf->writeByte('#');
            v->visit(this);
            if (idx != 0)
              buf->writeByte('0' + idx);
            buf->writeByte('#');
        } else
            v->visit(this);
    }
};

unsigned TypeBasic::nextIdx[TMAX] = {0};

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
        auto c_t = new cpp::TypeBasic(t->ty, T);
        assert(c_t == merge(c_t));
        toD[T] = merge(c_t);
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
    map(Context.Float128Ty, Type::tfloat80); // FIXME: there is no equivalent in D atm

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

/*****  *****/

// For D templates it is preferable to revert Calypso-specific basic types to a vanilla basic type
// It might hold some surprises in some advanced cases, but for most cases there will be less surprising errors, for example isIntegral!() failing because the TypeBasic isn't among the static list of integral types in std.traits.
Type* LangPlugin::typeForDTemplateArg(Type* t)
{
    if (t->isTypeBasic())
    {
        auto d_t = Type::basic[t->ty];
        return d_t->castMod(t->mod);
    }

    return t;
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

inline clang::QualType getPointeeType(const clang::Type* T)
{
    if (auto Reference = dyn_cast<clang::ReferenceType>(T))
        return Reference->getPointeeType();
    else if (auto Pointer = dyn_cast<clang::PointerType>(T))
        return Pointer->getPointeeType();
    else if (auto BlockPointer = dyn_cast<clang::BlockPointerType>(T)) // OS X extension
        return BlockPointer->getPointeeType();

    return clang::QualType();
}

Type *DeclMapper::fromType(const clang::QualType T, Loc loc)
{
    return FromType(*this, loc)(T);
}

DeclMapper::FromType::FromType(DeclMapper &mapper, Loc loc, TypeQualified *prefix)
    : mapper(mapper), loc(loc), prefix(prefix)
{
}

Type *DeclMapper::FromType::operator()(const clang::QualType _T)
{
    if (isNonSupportedType(_T))
        return nullptr;

    auto& Context = calypso.getASTContext();
    clang::QualType T = _T.getDesugaredType(Context);

    Type *t = fromTypeUnqual(T.getTypePtr());

    if (!t)
        return nullptr;

    if (T.isConstQualified())
        t = t->makeConst();

    // TODO: volatile qualifiers should be applied as attributes

    // restrict qualifiers are inconsequential

    return merge(t);
}

Type *DeclMapper::FromType::fromType(const clang::QualType T)
{
    return (*this)(T);
}

Type *DeclMapper::FromType::fromTypeUnqual(const clang::Type *T)
{
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
    TYPEMAP(SubstTemplateTypeParmPack)
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
        auto pointeeT = getPointeeType(T);
        auto pt = fromType(pointeeT);
        if (!pt)
            return nullptr;

        Type *t;
        if (Pointer || BlockPointer)
            t = new TypePointer(pt);
        else
            t = new TypeReference(pt, isa<clang::RValueReferenceType>(Reference));
        return t;
    }

    llvm::llvm_unreachable_internal("Unrecognized C++ type");
}


Type *DeclMapper::FromType::fromTypeBuiltin(const clang::BuiltinType *T)
{
    auto t = calypso.builtinTypes.toD[T];

    assert(t && "missing built-in type correspondance");
    return t;
}

Type *DeclMapper::FromType::fromTypeComplex(const clang::ComplexType *T)
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

Type* DeclMapper::FromType::fromTypeVector(const clang::VectorType* T)
{
    auto t = fromType(T->getElementType());
    if (!t)
        return nullptr;
    auto dim = new_IntegerExp(T->getNumElements());

    return new_TypeVector(new_TypeSArray(t, dim));
}

Type* DeclMapper::FromType::fromTypeArray(const clang::ArrayType* T)
{
    auto t = fromType(T->getElementType());
    if (!t)
        return nullptr;

    if (auto CAT = dyn_cast<clang::ConstantArrayType>(T))
    {
        auto dim = new_IntegerExp(CAT->getSize().getLimitedValue());
        return new_TypeSArray(t, dim);
    }
    else if (auto DSAT = dyn_cast<clang::DependentSizedArrayType>(T))
    {
        auto dim = ExprMapper(mapper).fromExpression(DSAT->getSizeExpr());
        return new_TypeSArray(t, dim);
    }
    else if (isa<clang::IncompleteArrayType>(T))
    {
        return new TypeIncompleteArray(t);
    }

    llvm::llvm_unreachable_internal("Unrecognized C++ array type");
}

// MSVC-specific(?) __underlying_type intrinsic returning the underlying integral type of an enum
Type* DeclMapper::FromType::fromTypeUnaryTransform(const clang::UnaryTransformType* T)
{
    auto Underlying = T->getUnderlyingType();
    if (!Underlying.isNull())
        return fromType(Underlying);

    return fromType(T->getBaseType()); // Underlying may be null if T is dependent
}

template<bool wantTuple>
  Objects* DeclMapper::FromType::fromTemplateArgument(const clang::TemplateArgument* Arg,
                const clang::NamedDecl *Param)
{
    ExprMapper expmap(mapper);
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
                tiarg = new_NullExp(loc/*, fromType(Arg->getNullPtrType())*/);
                break;
            case clang::TemplateArgument::Type:
                tiarg = FromType(mapper, loc)(Arg->getAsType());
                break;
            case clang::TemplateArgument::Template:
                tiarg = fromTemplateName(Arg->getAsTemplate());
                break;
            case clang::TemplateArgument::Pack:
                if (wantTuple)
                {
                    auto tup = new_Tuple();
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

template Objects *DeclMapper::FromType::fromTemplateArgument<false>(const clang::TemplateArgument *Arg,
                                                                                const clang::NamedDecl *Param);
template Objects *DeclMapper::FromType::fromTemplateArgument<true>(const clang::TemplateArgument *Arg,
                                                                                const clang::NamedDecl *Param);

namespace {
    // Sometimes e.g when mapping a TemplateSpecializationType template arguments might be unpacked
    // A smarter iterator is needed to iterate properly
    struct TemplateParameterIterator
    {
        clang::TemplateParameterList::const_iterator Param = nullptr,
            ParamEnd = nullptr;
        unsigned PackArgIdx = 0;
        unsigned NumArgsInOnePack;

        inline void adjustParam(clang::TemplateParameterList::const_iterator& P)
        {
            if (P == ParamEnd) {
                P = nullptr;
                return;
            }
            if (!NumArgsInOnePack)
                while (P && isTemplateParameterPack(*P))
                    adjustParam(++P);
        }

        TemplateParameterIterator(unsigned NumArgs, const clang::TemplateParameterList *ParamList)
        {
            if (!ParamList)
                return;

            unsigned NumParamPacks = 0;
            for (auto P : *ParamList)
                if (isTemplateParameterPack(P))
                    NumParamPacks++;

            NumArgsInOnePack = NumParamPacks ?
                ((2 * NumParamPacks + NumArgs - ParamList->size() - 1) / NumParamPacks) : 0;

            Param = ParamList->begin();
            ParamEnd = ParamList->end();
            adjustParam(Param);
        }

        TemplateParameterIterator& operator++() 
        {
            assert(Param);
            if (isTemplateParameterPack(*Param)) {
                PackArgIdx++;
                if (PackArgIdx < NumArgsInOnePack)
                    return *this;
                PackArgIdx = 0;
            }
            ++Param;
            adjustParam(Param);
            return *this;
        }

        const clang::NamedDecl* operator*() {
            return *Param;
        }
        operator bool() const { return Param != nullptr;  }
    };
}

template<bool wantTuple>
  Objects* DeclMapper::FromType::fromTemplateArguments(const clang::TemplateArgument *First,
                                        const clang::TemplateArgument *End,
                                        const clang::TemplateParameterList *ParamList)
{
    auto tiargs = new Objects;
    TemplateParameterIterator Param(End - First, ParamList);

    for (auto Arg = First; Arg != End; Arg++)
    {
        auto P = Param ? *Param : nullptr;
        auto arg = fromTemplateArgument<wantTuple>(Arg, P);
        assert(arg->dim || Arg->getKind() == clang::TemplateArgument::Pack);
        tiargs->append(arg);

        if (ParamList)
            ++Param;
    }

    return tiargs;
}

template
  Objects* DeclMapper::FromType::fromTemplateArguments<true>(const clang::TemplateArgument *First,
                                        const clang::TemplateArgument *End,
                                        const clang::TemplateParameterList *ParamList);

template<bool wantTuple>
  Objects *DeclMapper::fromTemplateArguments(Loc loc, const clang::TemplateArgumentList *List,
                                           const clang::TemplateParameterList *ParamList)
{
    auto Array = List->asArray();
    auto First = Array.begin(), End = Array.end();
    return FromType(*this, loc).fromTemplateArguments<wantTuple>(First, End, ParamList);
}

template Objects* DeclMapper::fromTemplateArguments<false>(Loc loc, const clang::TemplateArgumentList *List,
                                           const clang::TemplateParameterList *ParamList);
template Objects* DeclMapper::fromTemplateArguments<true>(Loc loc, const clang::TemplateArgumentList *List,
                                           const clang::TemplateParameterList *ParamList);

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
    DeclMapper::FromType &from;
    DeclMapper &mapper;

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
                  ::TemplateInstance *tempinst);

    void pushInst(TypeQualified *&tqual,
                RootObject *o,
                const clang::TemplateDecl *Temp,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd,
                clang::NamedDecl *Spec = nullptr);

    RootObject *getIdentOrTempinst(const clang::Decl *D);

    TypeQualifiedBuilder(DeclMapper::FromType &from, const clang::Decl* Root,
        const clang::NamedDecl *TopDecl = nullptr,
        const clang::TemplateArgument *TempArgBegin = nullptr,
        const clang::TemplateArgument *TempArgEnd = nullptr,
        TypeQualifiedBuilderOpts options = TQ_None)
        : from(from), mapper(from.mapper), options(options), RootEquals(Root),
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
        addInst(tqual, static_cast<::TemplateInstance*>(o));
}

void TypeQualifiedBuilder::addIdent(TypeQualified *&tqual,
                                    Identifier *ident)
{
    if (!tqual)
        tqual = new_TypeIdentifier(from.loc, ident);
    else
        tqual->addIdent(ident);
}

void TypeQualifiedBuilder::addInst(TypeQualified *&tqual,
                                   ::TemplateInstance *tempinst)
{
    if (!tqual)
        tqual = new_TypeInstance(from.loc, tempinst);
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

    ::TemplateInstance *tempinst;
    if (o->dyncast() == DYNCAST_IDENTIFIER)
    {
        auto ident = static_cast<Identifier*>(o);
        tempinst = new_TemplateInstance(loc, ident, tiargs);
    }
    else // templated overloaded operator
    {
        tempinst = static_cast<::TemplateInstance*>(o);
        tempinst->tiargs->append(tiargs);
    }

    addInst(tqual, tempinst);
}

RootObject *TypeQualifiedBuilder::getIdentOrTempinst(const clang::Decl *D)
{
    auto Named = dyn_cast<clang::NamedDecl>(D);
    if (!Named)
        return nullptr;

    if (options & TQ_OverOpFullIdent)
        return getExtendedIdentifierOrNull(Named, mapper);

    SpecValue spec(mapper); // overloaded operator
    auto ident = getIdentifierOrNull(Named, &spec);

    if (spec && !(options & TQ_OverOpSkipSpecArg))
    {
        auto loc = fromLoc(D->getLocation());
        auto tiargs = new Objects;
        tiargs->push(spec.toTemplateArg(from.loc));
        auto tempinst = new_TemplateInstance(loc, ident, tiargs);
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
            tqual = new_TypeIdentifier(from.loc, Id::empty); // start with the module scope operator . to protect against collisions

            if (auto m = mapper.dsymForDecl(TopDecl)->getModule())
            {
                std::function<void(Dsymbol*)> add = [&] (Dsymbol* pkg) {
                    if (auto parent = pkg->parent)
                        add(parent);
                    addIdent(tqual, pkg->ident);
                };
                add(m);
            }
            // if no Import was returned D is part of the module being mapped

            if (isa<clang::NamespaceDecl>(D) || isa<clang::TranslationUnitDecl>(D))
                return tqual;
        }
        else
        {
            auto Parent = cast<clang::Decl>(
                        getDeclContextOpaque(D));
            tqual = get(Parent);
        }
    }

    auto DC = getDeclContextOpaque(D);
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
        Spec = const_cast<clang::ClassTemplateSpecializationDecl*>(ClassSpec);
        SpecTemp = ClassSpec->getSpecializedTemplate();
        TempArgs = ClassSpec->getTemplateArgs().asArray();
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

TypeQualified *DeclMapper::FromType::typeQualifiedFor(clang::NamedDecl *D,
                        const clang::TemplateArgument *ArgBegin, const clang::TemplateArgument *ArgEnd,
                        TypeQualifiedBuilderOpts options)
{
    auto dsym = mapper.dsymForDecl(D);
    if (!ArgBegin && dsym) {
        assert(dsym->getType());
        return (TypeQualified*) dsym->getType(); // FIXME
    }

    auto mod = dsym->getModule();
    assert(mod && isCPP(mod));
    auto Root = static_cast<cpp::Module*>(mod)->rootDecl;
    if (!Root)
        return nullptr; // FIXME struct {} Val;

    return TypeQualifiedBuilder(*this, Root, D, ArgBegin, ArgEnd, options).get(D);
}

Type* DeclMapper::FromType::fromTypeTypedef(const clang::TypedefType* T)
{
    auto Typedef = T->getDecl();
    if (isAnonTagTypedef(Typedef) || isSameNameTagTypedef(Typedef))
        return fromType(Typedef->getUnderlyingType());

    return typeQualifiedFor(Typedef);
}

Type* DeclMapper::FromType::fromTypeEnum(const clang::EnumType* T)
{
    return typeQualifiedFor(T->getDecl());
}

Type *DeclMapper::FromType::fromTypeRecord(const clang::RecordType *T)
{
    return typeQualifiedFor(T->getDecl());
}

// Rarely used feature of C++, see [expr.mptr.oper]
// In the Itanium ABI a member func pointer is a pair of ptrdiff_t { ptr; thisadj; }
Type *DeclMapper::FromType::fromTypeMemberPointer(const clang::MemberPointerType *T)
{
    auto mt = fromType(T->getPointeeType());
    auto tc = FromType(mapper, loc).fromTypeUnqual(T->getClass());

    auto tiargs = new Objects;
    tiargs->push(mt); // we need to remember the member type and the parent class, in case we have to send the type back to Clang
    tiargs->push(tc);
    auto ti = new_TemplateInstance(loc, calypso.id_cpp_member_ptr, tiargs);

    auto t = new_TypeIdentifier(loc, Id::empty);
    t->addIdent(calypso.id_cpp);
    t->addIdent(calypso.id_core);
    t->addInst(ti);

    if (!T->isDependentType())
        return typeSemantic(t, Loc(), mapper.minst->_scope);
    else
        return t;
}

Type *DeclMapper::FromType::fromTypeElaborated(const clang::ElaboratedType *T)
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

    return FromType(mapper, loc, tqual)(T->getNamedType());
}

TypeQualified *DeclMapper::FromType::fromTemplateName(const clang::TemplateName Name,
                const clang::TemplateArgument *ArgBegin,
                const clang::TemplateArgument *ArgEnd)
{
    Identifier *tempIdent;

    switch (Name.getKind())
    {
        case clang::TemplateName::Template:
        {
            auto Temp = Name.getAsTemplateDecl();
            if (auto TempParm = dyn_cast<clang::TemplateTemplateParmDecl>(Temp))
                tempIdent = mapper.getIdentifierForTemplateTemplateParm(TempParm);
            else
                return typeQualifiedFor(Temp, ArgBegin, ArgEnd);
            break;
        }

        case clang::TemplateName::QualifiedTemplate:
        {
            TypeQualified *tqual = nullptr;
            auto NNS = Name.getAsQualifiedTemplateName()->getQualifier();

            if (NNS->getKind() == clang::NestedNameSpecifier::TypeSpec ||
                    NNS->getKind() == clang::NestedNameSpecifier::TypeSpecWithTemplate)
                tqual = fromNestedNameSpecifier(NNS);

            return FromType(mapper, loc, tqual).typeQualifiedFor(Name.getAsTemplateDecl(), ArgBegin, ArgEnd);
        }

        case clang::TemplateName::SubstTemplateTemplateParm:
            tempIdent = mapper.getIdentifierForTemplateTemplateParm(
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
        auto tiargs =fromTemplateArguments(ArgBegin, ArgEnd,
                                        Name.getAsTemplateDecl()->getTemplateParameters());
        auto ti = new_TemplateInstance(loc, tempIdent, tiargs);

        return new_TypeInstance(loc, ti);
    }
    else
        return new_TypeIdentifier(loc, tempIdent);
}

Type* DeclMapper::FromType::fromTypeTemplateSpecialization(const clang::TemplateSpecializationType* T)
{
    return fromTemplateName(T->getTemplateName(), T->begin(), T->end());
}

Identifier *DeclMapper::getIdentifierForTemplateTypeParm(const clang::TemplateTypeParmDecl *D,
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

    return Identifier::idPool(OS.str().c_str(), OS.str().size());
}

Identifier *DeclMapper::getIdentifierForTemplateTemplateParm(const clang::TemplateTemplateParmDecl *D)
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

    return Identifier::idPool(OS.str().c_str(), OS.str().size());
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

Type* DeclMapper::FromType::fromTypeTemplateTypeParm(const clang::TemplateTypeParmType* T)
{
    auto D = T->getDecl();
    auto ident = mapper.getIdentifierForTemplateTypeParm(D, T);
    return new_TypeIdentifier(loc, ident);
}

Type* DeclMapper::FromType::fromTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType* T)
{
    auto Replacement = T->getReplacementType();
    if (!Replacement.isNull())
        return fromType(Replacement);

    return fromTypeTemplateTypeParm(T->getReplacedParameter());
}

Type* DeclMapper::FromType::fromTypeSubstTemplateTypeParmPack(const clang::SubstTemplateTypeParmPackType* T)
{
    auto Pack = T->getArgumentPack();
    auto args = fromTemplateArgument<false>(&Pack);
    if (args && args->dim){
        if (args->dim > 1)
            ::warning(loc, "FIXME: skipping template arguments from SubstTemplateTypeParmPackType");
        return isType((*args)[0]);
    }
    return nullptr;
}

Type* DeclMapper::FromType::fromTypeInjectedClassName(const clang::InjectedClassNameType* T) // e.g in template <...> class A { A &next; } next has an injected class name type
{
    // A simple identifier is preferrable when possible, as mapping template arguments requires switching to a different parameter scope
    return new_TypeIdentifier(loc, getIdentifier(T->getDecl()));

//     return typeQualifiedFor(T->getDecl());
        // NOTE: this will return typeof(this) if we aren't in a nested class, but if we are the name of the record is (without template arguments)
}

TypeQualified *DeclMapper::FromType::fromNestedNameSpecifierImpl(const clang::NestedNameSpecifier *NNS)
{
    TypeQualified *result = nullptr;

    switch (NNS->getKind())
    {
        case clang::NestedNameSpecifier::Identifier:
        {
            auto ident = fromIdentifier(NNS->getAsIdentifier());
            result = new_TypeIdentifier(loc, ident);
            break;
        }

        case clang::NestedNameSpecifier::TypeSpec:
        case clang::NestedNameSpecifier::TypeSpecWithTemplate:
        {
            auto t = FromType(mapper, loc, nullptr).fromTypeUnqual(NNS->getAsType());
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

TypeQualified* DeclMapper::FromType::fromNestedNameSpecifier(const clang::NestedNameSpecifier* NNS)
{
    if (auto Prefix = NNS->getPrefix())
        if (auto tqual = fromNestedNameSpecifier(Prefix))
            return FromType(mapper, loc, tqual).fromNestedNameSpecifierImpl(NNS);

    return fromNestedNameSpecifierImpl(NNS);
}

// NOTE: Dependent***Type are not mandatory to get templates working because the instantiation is done by Sema
// and then DMD simply maps the resulting class or function specialization, so we could return TypeNull and it would still work.
// Still good for reflection.
Type* DeclMapper::FromType::fromTypeDependentName(const clang::DependentNameType* T)
{
    TypeQualified *tqual = nullptr;

    if (auto NNS = T->getQualifier())
        tqual = fromNestedNameSpecifier(NNS);

    auto ident = fromIdentifier(T->getIdentifier());
    if (!tqual)
        tqual = new_TypeIdentifier(loc, ident);
    else
        tqual->addIdent(ident);

    return tqual;
}

Type* DeclMapper::FromType::fromTypeDependentTemplateSpecialization(const clang::DependentTemplateSpecializationType* T)
{
    TypeQualified *tqual = nullptr;

    if (auto NNS = T->getQualifier())
        tqual = fromNestedNameSpecifier(NNS);

    auto ident = fromIdentifier(T->getIdentifier());
    auto tiargs = fromTemplateArguments(T->begin(), T->end());

    auto tempinst = new_TemplateInstance(loc, ident, tiargs);

    if (!tqual)
        tqual = new_TypeInstance(loc, tempinst);
    else
        tqual->addInst(tempinst);

    return tqual;
}

template <typename _Type>
Type *DeclMapper::FromType::fromTypeOfExpr(const _Type *T)
{
    if (T->isSugared())  // TODO: remove this for reflection?
    {
        FromType underlying(mapper, loc);
        underlying.TypeOfExpr = T->getUnderlyingExpr(); // needed for SubstTemplateTypeParm

        return underlying(T->desugar());
    }

    auto exp = ExprMapper(mapper).fromExpression(T->getUnderlyingExpr());
    assert(exp);
    return new_TypeTypeof(loc, exp);
}

Type* DeclMapper::FromType::fromTypeTypeOfExpr(const clang::TypeOfExprType* T)
{
    return fromTypeOfExpr(T);
}

Type* DeclMapper::FromType::fromTypeDecltype(const clang::DecltypeType* T)
{
    return fromTypeOfExpr(T);
}

Type* DeclMapper::FromType::fromTypePackExpansion(const clang::PackExpansionType* T)
{
    return fromType(T->getPattern());
}

TypeFunction *DeclMapper::FromType::fromTypeFunction(const clang::FunctionProtoType* T,
        const clang::FunctionDecl *FD)
{
    auto& S = calypso.getSema();
//     auto& Diags = calypso.getDiagnostics();

    auto params = new Parameters;
    params->reserve(T->getNumParams());

    decltype(FD->param_begin()) PI;
    if (FD)
        PI = FD->param_begin();

    for (auto I = T->param_type_begin(), E = T->param_type_end();
                I != E; I++)
    {
        StorageClass stc = STCundefined;
        auto at = mapper.fromType(*I, loc);
        Identifier *ident = nullptr;
        Expression *defaultArg = nullptr;

        if (!at)
            return nullptr;

        auto Pointee = getPointeeType(I->getTypePtr());
        if (I->isVolatileQualified() ||
                (!Pointee.isNull() && Pointee.isVolatileQualified()))
            stc |= STCvolatile;

        // Turn a TypeReference into « ref nextOf() » as early as possible as this helps function resolving
        if (at->ty == Treference)
        {
            auto tref = static_cast<TypeReference*>(at);
            stc |= /*STCscope | */STCref;
            if (tref->isRvalRef())
                stc |= STCmove;
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
                    defaultArg = ExprMapper(mapper).fromExpression(DefaultArgExpr);

                if (defaultArg && !T->isDependentType() && mapper.minst->_scope) // FIXME: this is temporary until ExprMapper always return semantic'd expressions
                    defaultArg = expressionSemantic(defaultArg, mapper.minst->_scope);

//                 if (Diags.hasErrorOccurred())
//                     Diags.Reset();
            }

            PI++;
        }

        params->push(new_Parameter(stc, at, ident, defaultArg, nullptr));
    }

    StorageClass stc = STCundefined;
    if (T->isConst())
        stc |= STCconst;
    if (T->isVolatile())
        stc |= STCvolatile;

    Type *rt;
    auto AutoTy = dyn_cast<clang::AutoType>(T->getReturnType());
    if (AutoTy && !AutoTy->isSugared()) {
        stc |= STCauto;
        rt = nullptr;
    } else {
        rt = FromType(mapper, loc)(T->getReturnType());
        if (!rt)
            return nullptr;

        if (rt->ty == Treference)
        {
            auto tref = static_cast<TypeReference*>(rt);
            stc |= STCref;
            if (tref->isRvalRef())
                stc |= STCmove;
            rt = rt->nextOf();
        }
        else if (FD && isa<clang::CXXConstructorDecl>(FD))
        {
            // Tweak constructor types to match DMD's
            auto ad = static_cast<AggregateDeclaration*>(
                    mapper.dsymForDecl(cast<clang::CXXConstructorDecl>(FD)->getParent()));
            rt = ad->handleType();
            stc |= STCref;
        }
    }

    if (!clang::isUnresolvedExceptionSpec(T->getExceptionSpecType()) && T->isNothrow(false))
        stc |= STCnothrow;

    LINK linkage = (FD && FD->isExternC()) ? LINKc : LINKcpp;
        // TODO: inferring the linkage for overriding methods would be nice

    auto tf = new_TypeFunction(ParameterList{params, VARARGnone}, rt, linkage, stc);
    tf = static_cast<TypeFunction*>(tf->addSTC(stc));

    if (!T->isDependentType())
        tf->deco = merge(tf)->deco;
    return tf;
}

// // Build and possibly add imports for symbols referenced in the module being mapped
// cpp::Import *DeclMapper::AddImplicitImportForDecl(Loc loc, const clang::NamedDecl *D, bool fake)
// {
//     auto Key = GetImplicitImportKeyForDecl(D);
//
//     assert(fake || !mod || isCPP(mod));
//
//     cpp::Import *im = BuildImplicitImport(loc, Key, nullptr);
//     return im;
// }
//
// const clang::Decl* DeclMapper::GetImplicitImportKeyForDecl(const clang::NamedDecl* D)
// {
//     D = cast<clang::NamedDecl>(getCanonicalDecl(D));
//
//     if (D->getFriendObjectKind() != clang::Decl::FOK_None && D->isOutOfLine())
//         return GetImplicitImportKeyForDecl(  // friend declarations which aren't redeclared in the semantic declctx are part of the record module
//                 cast<clang::NamedDecl>(D->getLexicalDeclContext()));
//
//     auto TopMost = GetNonNestedContext(D);
//     bool IsNamespaceOrTU = isa<clang::TranslationUnitDecl>(TopMost) ||
//                     isa<clang::NamespaceDecl>(TopMost);
//
//     auto Func = dyn_cast<clang::FunctionDecl>(D);
//     if (auto FuncTemp = dyn_cast<clang::FunctionTemplateDecl>(D))
//         Func = FuncTemp->getTemplatedDecl();
//     if (IsNamespaceOrTU && Func && Func->isOverloadedOperator())
//         if (auto Tag = isOverloadedOperatorWithTagOperand(D)) // non-member operators are part of the record module
//             return GetImplicitImportKeyForDecl(Tag);
//
//     if (auto Spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(TopMost))
//         return GetImplicitImportKeyForDecl(Spec->getSpecializedTemplate());
//
//     return TopMost->getCanonicalDecl();
// }

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

        auto Parent = cast<clang::Decl>(getDeclContextOpaque(D));
        auto TagParent = cast<clang::Decl>(getDeclContextOpaque(Tag));

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
                        getDeclContextOpaque(D)));
}

// static Identifier *BuildImplicitImportInternal(const clang::DeclContext *DC,
//                                                Loc loc, Identifiers *sPackages)
// {
//     if (DC->isTranslationUnit()) return nullptr;
//     assert(!DC->isFunctionOrMethod() && "Building import for a decl nested inside a func?");
//
//     if (auto sModule = BuildImplicitImportInternal(
//                 getDeclContextOpaque(cast<clang::Decl>(DC)), loc, sPackages))
//         return sModule;
//
//     if (auto NS = dyn_cast<clang::NamespaceDecl>(DC))
//     {
//         if (NS->isAnonymousNamespace())
//             error(loc, "Cannot import symbols from anonymous namespaces");
//
//         if (!NS->isInline())
//             sPackages->push(getIdentifier(NS));
//
//         return nullptr;
//     }
//     else if (isa<clang::TagDecl>(DC))
//         return getIdentifier(cast<clang::NamedDecl>(DC));
//
//     llvm_unreachable("Unhandled case");
// }
//
// cpp::Import *DeclMapper::BuildImplicitImport(Loc loc, const clang::Decl *D, Identifier *aliasid)
// {
//     auto DC = cast<clang::DeclContext>(D);
//
//     auto sPackages = new Identifiers;
//     auto sModule = BuildImplicitImportInternal(DC, loc, sPackages);
//
//     if (!sModule)
//     {
//         if (isa<clang::ClassTemplateDecl>(D))
//             sModule = getIdentifier(cast<clang::NamedDecl>(D));
//         else
//             // D is neither a tag nor a class template, we need to import the namespace's functions and vars
//             sModule = calypso.id__;
//     }
//
//     return new cpp::Import(loc, sPackages, sModule, aliasid, 1);
// }

/***** DMD -> Clang types *****/

clang::QualType DeclMapper::toType(Loc loc, Type* t, Scope *sc, StorageClass stc)
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
        case Tclass:
        {
            // Special treatment of __cpp_member_ptr!(T, Cls)
            auto ad = getAggregateSym(t);
            if (ad->ident == calypso.id_cpp_member_ptr)
            {
                auto ti = ad->toParent()->isTemplateInstance();
                assert(ti && ti->tiargs->dim >= 2);
                auto memberty = isType((*ti->tiargs)[0]);
                auto tagg = isType((*ti->tiargs)[1]);
                assert(tagg && (tagg->ty == Tstruct || tagg->ty == Tclass) && isCPP(getAggregateSym(tagg)));

                auto MemberTy = toType(loc, memberty, sc);
                auto ClassParent = toType(loc, tagg, sc);

                return Context.getMemberPointerType(MemberTy, ClassParent.getTypePtr());
            }

            if (!isCPP(ad))
                return clang::QualType();

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
            t = typeSemantic(t, loc, sc);
            return toType(loc, t, sc, stc);
        }
        case Tpointer:
        case Treference:
        {
            auto Pointee = toType(loc, t->nextOf(), sc);

            if (t->ty == Tpointer) {
                if (isCPP(t))
                    if (static_cast<TypePointer*>(t)->isIncompleteArray())
                        return Context.getIncompleteArrayType(Pointee, clang::ArrayType::Normal, 0);
                return Context.getPointerType(Pointee);
            } else {
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
            if (tf->parameterList.parameters)
                for (auto p: *tf->parameterList.parameters)
                    Args.push_back(toType(loc, p->type, sc, p->storageClass));

            clang::FunctionProtoType::ExtProtoInfo EPI;
            return Context.getFunctionType(ResultTy, Args, EPI);
        }
        // TODO arrays
    }

    return clang::QualType();
//     llvm::llvm_unreachable_internal("Unhandled D -> Clang type conversion");
}

/***** *****/

// NOTE: doesn't return null if the template isn't defined. What we really want is some sort of canonical declaration to refer to for template parameter names.
template <typename RedeclTempDecl>
 const RedeclTempDecl *getRTDDefinition(const RedeclTempDecl *D)
{
    for (auto RI: D->redecls()) // find the definition if any
    {
        auto I = cast<RedeclTempDecl>(RI);
        if (I->isThisDeclarationADefinition())
            return I;
    }

    return cast<RedeclTempDecl>(getCanonicalDecl(D));
}

const clang::RedeclarableTemplateDecl* getDefinition(const clang::RedeclarableTemplateDecl* D)
{
    if (auto CTD = dyn_cast<clang::ClassTemplateDecl>(D))
        return getRTDDefinition(CTD);
    else if (auto FTD = dyn_cast<clang::FunctionTemplateDecl>(D))
        return getRTDDefinition(FTD);
    else if (auto VTD = dyn_cast<clang::VarTemplateDecl>(D))
        return getRTDDefinition(VTD);
    return D;
}

const clang::ClassTemplateSpecializationDecl *getDefinition(const clang::ClassTemplateSpecializationDecl *D)
{
    if (auto Definition = D->getDefinition())
        return cast<clang::ClassTemplateSpecializationDecl>(Definition);

    return D;
}

const clang::VarTemplateSpecializationDecl *getDefinition(const clang::VarTemplateSpecializationDecl * D)
{
    if (auto Definition = D->getDefinition())
        return cast<clang::VarTemplateSpecializationDecl>(Definition);

    return D;
}

// skips linkage specs, inline namespaces, anonymous enums, and "true" anonymous structs or unions
const clang::DeclContext *getDeclContextOpaque(const clang::Decl *D)
{
    auto DC = D->getDeclContext();

    while (isa<clang::LinkageSpecDecl>(DC) || DC->isInlineNamespace())
        DC = DC->getParent();

    auto NamedDC = dyn_cast<clang::NamedDecl>(DC);
    if (NamedDC && NamedDC->getDeclName().isEmpty())
    {
        if (auto Tag = dyn_cast<clang::TagDecl>(DC))
            if (Tag->getTypedefNameForAnonDecl())
                return DC;

        if (auto Record = dyn_cast<clang::RecordDecl>(DC))
            if (!Record->isAnonymousStructOrUnion())
                return DC;

        return getDeclContextOpaque(NamedDC);
    }

    assert(NamedDC || isa<clang::TranslationUnitDecl>(DC));

    return DC;
}

// Helper function for GDB
const clang::Decl *getParent(const clang::Decl *D)
{
    return cast<clang::Decl>(D->getDeclContext());
}

// WARNING: currently Calypso doesn't map explicit specializations as TemplateDeclaration
// anymore, so getSpecializedDeclOrExplicit is a misnomer, but kept as a reminder
// and in case it starts mapping as TemplateDeclaration again for reflection
const clang::Decl *getSpecializedDeclOrExplicit(const clang::Decl *Spec)
{
    if (auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(Spec))
    {
        if (/*ClassSpec->isExplicitSpecialization() ||
                */isa<clang::ClassTemplatePartialSpecializationDecl>(Spec))
            return Spec;

        auto U = ClassSpec->getSpecializedTemplateOrPartial();

        if (U.is<clang::ClassTemplateDecl*>())
            return U.get<clang::ClassTemplateDecl*>();
        else
            return U.get<clang::ClassTemplatePartialSpecializationDecl*>();
    }
    else if (auto VarSpec = dyn_cast<clang::VarTemplateSpecializationDecl>(Spec))
    {
        if (/*VarSpec->isExplicitSpecialization() ||
                */isa<clang::VarTemplatePartialSpecializationDecl>(Spec))
            return Spec;

        auto U = VarSpec->getSpecializedTemplateOrPartial();

        if (U.is<clang::VarTemplateDecl*>())
            return U.get<clang::VarTemplateDecl*>();
        else
            return U.get<clang::VarTemplatePartialSpecializationDecl*>();
    }
    else if (auto FuncSpec = dyn_cast<clang::FunctionDecl>(Spec))
    {
        assert(FuncSpec->getPrimaryTemplate());

//         if (FuncSpec->getTemplateSpecializationKind() == clang::TSK_ExplicitSpecialization)
//             return Spec;

        return FuncSpec->getPrimaryTemplate();
    }

    return Spec;
//     llvm_unreachable("Not a template spec?");
}

}
