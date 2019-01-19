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
#include "id.h"
#include "identifier.h"
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
    bool isMergeable() override { return false; }
    unsigned short sizeType() override { return sizeof(*this); }

    virtual bool isIncompleteArray() const { return false; }
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

    unsigned short sizeType() override { return sizeof(*this); }
    bool isIncompleteArray() const override { return true; }

    void accept(Visitor *v) override
    {
        auto v_ti = v->_typeid();

        if (v_ti == TI_Mangler) { // mangle
            auto buf = static_cast<Mangler*>(v)->buf;

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
    bool isMergeable() override { return false; }
    unsigned short sizeType() override { return sizeof(*this); }

    void accept(Visitor *v) override
    {
        auto v_ti = v->_typeid();

        if (v_ti == TI_Mangler) { // mangle
            if (isRvalueRef) {
                v->visit(static_cast<Type*>(this)); // only 'R'
                static_cast<Mangler*>(v)->buf->writeByte('#');
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

    unsigned short sizeType() override
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
        return t;
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
    auto dim = new_IntegerExp(T->getNumElements());

    return new_TypeVector(new_TypeSArray(t, dim));
}

Type* TypeMapper::FromType::fromTypeArray(const clang::ArrayType* T)
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
        auto dim = ExprMapper(tm).fromExpression(DSAT->getSizeExpr());
        return new_TypeSArray(t, dim);
    }
    else if (isa<clang::IncompleteArrayType>(T))
    {
        return new TypeIncompleteArray(t);
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
                tiarg = new_NullExp(loc/*, fromType(Arg->getNullPtrType())*/);
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

template Objects *TypeMapper::FromType::fromTemplateArgument<false>(const clang::TemplateArgument *Arg,
                                                                                const clang::NamedDecl *Param);
template Objects *TypeMapper::FromType::fromTemplateArgument<true>(const clang::TemplateArgument *Arg,
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
  Objects* TypeMapper::FromType::fromTemplateArguments(const clang::TemplateArgument *First,
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
  Objects* TypeMapper::FromType::fromTemplateArguments<true>(const clang::TemplateArgument *First,
                                        const clang::TemplateArgument *End,
                                        const clang::TemplateParameterList *ParamList);

template<bool wantTuple>
  Objects *TypeMapper::fromTemplateArguments(Loc loc, const clang::TemplateArgumentList *List,
                                           const clang::TemplateParameterList *ParamList)
{
    auto Array = List->asArray();
    auto First = Array.begin(), End = Array.end();
    return FromType(*this, loc).fromTemplateArguments<wantTuple>(First, End, ParamList);
}

template Objects* TypeMapper::fromTemplateArguments<false>(Loc loc, const clang::TemplateArgumentList *List,
                                           const clang::TemplateParameterList *ParamList);
template Objects* TypeMapper::fromTemplateArguments<true>(Loc loc, const clang::TemplateArgumentList *List,
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
                  ::TemplateInstance *tempinst);

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
        return getExtendedIdentifierOrNull(Named, tm);

    SpecValue spec(tm); // overloaded operator
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
            tqual = new_TypeIdentifier(from.loc, Id::empty); // start with the module scope operator . to guarantee against collisions

            // build a fake import
            if (auto im = tm.AddImplicitImportForDecl(from.loc, TopDecl, true))
            {
                if (im->aliasId)
                    addIdent(tqual, im->aliasId);
                else
                {
                    for (auto package: *im->packages)
                        addIdent(tqual, package);
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
    return new_TypeTypeof(loc, new_ThisExp(loc));
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
        return new_TypeIdentifier(fromLoc(D->getLocation()), getIdentifier(D));

    if (!ArgBegin && (options & TQ_PreferCachedSym) && D->dsym) {
        assert(D->dsym->getType());
        return (TypeQualified*) D->dsym->getType(); // FIXME
    }

    auto Root = tm.GetRootForTypeQualified(D);
    if (!Root)
        return nullptr; // FIXME struct {} Val;

    tm.AddImplicitImportForDecl(loc, D);
    return TypeQualifiedBuilder(*this, Root, D, ArgBegin, ArgEnd, options).get(D);
}

Type* TypeMapper::FromType::fromTypeTypedef(const clang::TypedefType* T)
{
    auto Typedef = T->getDecl();
    if (isAnonTagTypedef(Typedef) || isSameNameTagTypedef(Typedef))
        return fromType(Typedef->getUnderlyingType());

    return typeQualifiedFor(Typedef);
}

Type* TypeMapper::FromType::fromTypeEnum(const clang::EnumType* T)
{
    return typeQualifiedFor(T->getDecl(), nullptr, nullptr, TQ_PreferCachedSym);
}

Type *TypeMapper::FromType::fromTypeRecord(const clang::RecordType *T)
{
    return typeQualifiedFor(T->getDecl(), nullptr, nullptr, TQ_PreferCachedSym);
}

// Rarely used feature of C++, see [expr.mptr.oper]
// In the Itanium ABI a member func pointer is a pair of ptrdiff_t { ptr; thisadj; }
Type *TypeMapper::FromType::fromTypeMemberPointer(const clang::MemberPointerType *T)
{
    auto mt = fromType(T->getPointeeType());
    auto tc = FromType(tm, loc).fromTypeUnqual(T->getClass());

    auto tiargs = new Objects;
    tiargs->push(mt); // we need to remember the member type and the parent class, in case we have to send the type back to Clang
    tiargs->push(tc);
    auto ti = new_TemplateInstance(loc, calypso.id_cpp_member_ptr, tiargs);

    auto t = new_TypeIdentifier(loc, Id::empty);
    t->addIdent(calypso.id_cpp);
    t->addIdent(calypso.id_core);
    t->addInst(ti);
    return t;
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

// FIXME: the proper fix would be to be as lazy as C++, both during semantic phases and codegen.
// This means lazier than D, which could go through semantic phases lazily for imported modules but cannot be lazy for
// for modules being codegen'd.

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
        {
            auto Temp = Name.getAsTemplateDecl();
            if (auto TempParm = dyn_cast<clang::TemplateTemplateParmDecl>(Temp))
                tempIdent = tm.getIdentifierForTemplateTemplateParm(TempParm);
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
        auto tiargs =fromTemplateArguments(ArgBegin, ArgEnd,
                                        Name.getAsTemplateDecl()->getTemplateParameters());
        auto ti = new_TemplateInstance(loc, tempIdent, tiargs);

        return new_TypeInstance(loc, ti);
    }
    else
        return new_TypeIdentifier(loc, tempIdent);
}

Type* TypeMapper::FromType::fromTypeTemplateSpecialization(const clang::TemplateSpecializationType* T)
{
    auto tqual = fromTemplateName(T->getTemplateName(),
                            T->begin(), T->end());

    if (!tqual)
        return nullptr; // discard types leading to recursive template expansion

    if (T->isTypeAlias())
        ;
    else if (T->isSugared())
    {
        if (auto RT = T->getAs<clang::RecordType>()) {
            if (tm.isInjectedClassName(RT->getDecl()))
                return fromInjectedClassName(loc);
        }
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

    return Identifier::idPool(OS.str().c_str(), OS.str().size());
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

const clang::TemplateTypeParmDecl *TypeMapper::FromType::getOriginalTempTypeParmDecl(const clang::TemplateTypeParmType *T)
{
    auto ParmDecl = T->getDecl();
    if (!ParmDecl) {
        auto Depth = T->getDepth();
        if (Depth < tm.TempParamScope.size())
            return cast<clang::TemplateTypeParmDecl>(tm.TempParamScope[Depth]->getParam(T->getIndex()));
        else
            return nullptr; // may happen for partial template specs arguments (meant for the master template params)
    }

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
    return new_TypeIdentifier(loc, ident);
}

Type* TypeMapper::FromType::fromTypeSubstTemplateTypeParm(const clang::SubstTemplateTypeParmType* T)
{
    auto Replacement = T->getReplacementType();
    if (!Replacement.isNull())
        return fromType(Replacement);

    auto OrigParm = getOriginalTempTypeParmDecl(T->getReplacedParameter());
    return fromTypeTemplateTypeParm(T->getReplacedParameter(), OrigParm);
}

Type* TypeMapper::FromType::fromTypeSubstTemplateTypeParmPack(const clang::SubstTemplateTypeParmPackType* T)
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

Type* TypeMapper::FromType::fromTypeInjectedClassName(const clang::InjectedClassNameType* T) // e.g in template <...> class A { A &next; } next has an injected class name type
{
    // A simple identifier is preferrable when possible, as mapping template arguments requires switching to a different parameter scope
    return new_TypeIdentifier(loc, getIdentifier(T->getDecl()));

//     return typeQualifiedFor(T->getDecl());
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
            result = new_TypeIdentifier(loc, ident);
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
        tqual = new_TypeIdentifier(loc, ident);
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

    auto tempinst = new_TemplateInstance(loc, ident, tiargs);

    if (!tqual)
        tqual = new_TypeInstance(loc, tempinst);
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
    return new_TypeTypeof(loc, exp);
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
            auto tref = static_cast<TypeReference*>(at);
            stc |= STCscope | STCref;
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
                    defaultArg = ExprMapper(tm).fromExpression(DefaultArgExpr);

                if (Diags.hasErrorOccurred())
                    Diags.Reset();
            }

            PI++;
        }

        params->push(new_Parameter(stc, at, ident, defaultArg, nullptr));
    }

    StorageClass stc = STCundefined;
    if (T->isConst())
        stc |= STCconst;

    Type *rt;
    auto AutoTy = dyn_cast<clang::AutoType>(T->getReturnType());
    if (AutoTy && !AutoTy->isSugared()) {
        stc |= STCauto;
        rt = nullptr;
    } else {
        rt = FromType(tm, loc)(T->getReturnType());
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
    }

    if (!clang::isUnresolvedExceptionSpec(T->getExceptionSpecType()) && T->isNothrow(Context, false))
        stc |= STCnothrow;

    LINK linkage = (FD && FD->isExternC()) ? LINKc : LINKcpp;
        // TODO: inferring the linkage for overriding methods would be nice

    auto tf = new_TypeFunction(params, rt, 0, linkage, stc);
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
cpp::Import *TypeMapper::AddImplicitImportForDecl(Loc loc, const clang::NamedDecl *D, bool fake)
{
    auto Key = GetImplicitImportKeyForDecl(D);

    if (mod && Key == mod->rootKey)
        return nullptr; // do not import self

    cpp::Import *im = implicitImports[Key].im;
    if (!im)
    {
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
                llvm::SmallString<48> s(u8"ℂ"); // non-ASCII but pretty
                s += llvm::StringRef(importIdent->toChars(), importIdent->length());
                importAliasid = Identifier::idPool(s.c_str(), s.size()); }

                assert(!Key.second);
                break;
            }
        }

        if (Key.second)
            im = BuildImplicitImport(loc, Key.first, Key.second, importAliasid);
        else
            im = BuildImplicitImport(loc, Key.first, importAliasid);

        implicitImports[Key].im = im;
    }

    if (!fake && !implicitImports[Key].added) {
        if (addImplicitDecls) {
            mod->members->shift(im);
            implicitImports[Key].added = true;
        } else if (scSemImplicitImports) {
            auto dst = Package::resolve(im->packages, nullptr, &im->pkg);
            if (!dst->lookup(im->id)) {
                dsymbolSemantic(im, scSemImplicitImports);
                semantic2(im, scSemImplicitImports);
            }
        }
    }

    return im;
}

Module::RootKey TypeMapper::GetImplicitImportKeyForDecl(const clang::NamedDecl* D)
{
    D = cast<clang::NamedDecl>(getCanonicalDecl(D));

    if (D->getFriendObjectKind() != clang::Decl::FOK_None && D->isOutOfLine())
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

cpp::Import *TypeMapper::BuildImplicitImport(Loc loc, const clang::Decl *D, Identifier *aliasid)
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
            sModule = calypso.id__;
    }

    return new cpp::Import(loc, sPackages, sModule, aliasid, 1);
}

cpp::Import *TypeMapper::BuildImplicitImport(Loc loc, const clang::Decl *D, const clang::Module *Mod,
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
    auto sModule = Identifier::idPool(Mod->Name.c_str(), Mod->Name.size());

    auto M = Mod->Parent;
    while (M)
    {
        sPackages->insert(insertIndex, Identifier::idPool(M->Name.c_str(), M->Name.size()));
        M = M->Parent;
    }

    return new cpp::Import(loc, sPackages, sModule, aliasid, 1);
}

void TypeMapper::pushTempParamList(const clang::Decl *D)
{
    const clang::TemplateParameterList *TPL = nullptr;

    if (auto RD = dyn_cast<clang::CXXRecordDecl>(D))
        if (auto ClassTemp = RD->getDescribedClassTemplate())
            D = ClassTemp;

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

    //if (auto ClassSpec = dyn_cast<clang::ClassTemplateSpecializationDecl>(D))
    //{
    //    if (!ClassSpec->isExplicitSpecialization())
    //        pushTempParamList(getSpecializedDeclOrExplicit(ClassSpec));
    //}
    //else if (auto Func = dyn_cast<clang::FunctionDecl>(D))
    //{
    //    if (auto PrimTemp = Func->getPrimaryTemplate())
    //        pushTempParamList(PrimTemp);
    //}
}

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
            if (tf->parameters)
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

TypeMapper::TypeMapper(cpp::Module* mod, bool isGlobal)
    : mod(mod), isGlobal(isGlobal)
{
}

TypeMapper::~TypeMapper()
{
    if (isGlobal)
        return; // IrDsymbol::list may get destroyed before this dtor call, and ~IrDsymbol would then cause a segfault

    for (auto impPair: implicitImports)
        if (!impPair.second.added) {
            auto im = impPair.second.im;
            if (im->packages) delete im->packages;
            delete im;
        }
}

TypeMapper::TempParamListRAII::TempParamListRAII(TypeMapper *tm, 
                const clang::TemplateParameterList * ParamList)
    : tm(tm)
{
    tm->TempParamScope.push_back(ParamList);
}

TypeMapper::TempParamListRAII::~TempParamListRAII()
{
    tm->TempParamScope.pop_back();
}

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
