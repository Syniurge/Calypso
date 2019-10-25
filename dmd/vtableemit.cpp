
#include "aggregate.h"
#include "attrib.h"
#include "declaration.h"
#include "enum.h"
#include "import.h"
#include "module.h"
#include "mtype.h"
#include "statement.h"
#include "template.h"

// CALYPSO trick the C++ compiler into emitting vtable symbols with C++ mangling for extern(C++) classes
// The rule GCC and Clang follows is to emit the vtable if the first non-inline virtual method is defined (outside the class declaration) in the translation unit

// Why is this required and NOTE on C++ interop issue: for extern(C++) classes D compilers currently mangle the vtbl à la D, so extern(C++) classes
// with virtual methods declared in .h headers but implemented in .d cannot be derived in C++ code without such a workaround.

void RootObject::_key() {}

void Type::_key() {}
void TypeBasic::_key() {}
void TypeNext::_key() {}
void TypePointer::_key() {}
void TypeReference::_key() {}

void Dsymbol::_key() {}
void ScopeDsymbol::_key() {}
void Declaration::_key() {}
void VarDeclaration::_key() {}
void FuncDeclaration::_key() {}
void CtorDeclaration::_key() {}
void DtorDeclaration::_key() {}
void FuncAliasDeclaration::_key() {}
void EnumDeclaration::_key() {}
void EnumMember::_key() {}
void AliasDeclaration::_key() {}
void Import::_key() {}
void AggregateDeclaration::_key() {}
void StructDeclaration::_key() {}
void UnionDeclaration::_key() {}
void ClassDeclaration::_key() {}
void Package::_key() {}
void Module::_key() {}
void TemplateDeclaration::_key() {}
void TemplateInstance::_key() {}

void Catch::_key() {}
void AttribDeclaration::_key() {}
void AnonDeclaration::_key() {}

// Define RootObject::toString() to make the vtable generation possible by C++ compilers
// Keep in mind that this is the function that ends up in vtables of C++ classes derived from D ones
DString RootObject::toString()
{
    DString result;
    result.ptr = this->toChars();
    result.length = strlen(result.ptr);
    return result;
}
