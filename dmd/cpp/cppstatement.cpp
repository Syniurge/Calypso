// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cppstatement.h"
#include "cpp/ddmdstructor.h"

#include "errors.h"

namespace cpp
{

Catch::Catch(Loc loc, Type *t, Identifier *id, Statement *handler, StorageClass stc)
{
    construct_Catch(this, loc, t, id, handler);
    this->stc = stc;
}

::Catch *Catch::syntaxCopy()
{
    Catch *c = new Catch(loc,
        type ? type->syntaxCopy() : NULL,
        ident,
        (handler ? handler->syntaxCopy() : NULL),
        stc);
    c->internalCatch = internalCatch;
    return c;
}

VarDeclaration *Catch::createVar(Identifier* id, StorageClass)
{
    if (stc & ~STCref)
    {
        error(loc, "Only ref storage class accepted in catch (C++) types");
        return nullptr;
    }

    if (!(stc & STCref) && isAggregateValue(type))
    {
        error(loc, "Aggregate types in catch (C++) must be caught by ref (for the time being)");
        return nullptr;
    }

    auto vd = ::Catch::createVar(id, stc);
    vd->storage_class |= stc | STCforeach /* HACK */;
    return vd;
}

bool LangPlugin::doesHandleCatch(LINK lang)
{
    return lang == LINKcpp;
}

::Catch *LangPlugin::createCatch(Loc loc, Type *t,
                        Identifier *id, Statement *handler,
                        StorageClass stc)
{
    return new Catch(loc, t, id, handler, stc);
}

}
