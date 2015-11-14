// Contributed by Elie Morisse, same license DMD uses

#include "cpp/cppstatement.h"

namespace cpp
{

Catch::Catch(Loc loc, Type *t, Identifier *id, Statement *handler)
        : ::Catch(loc, t, id, handler)
{
}

::Catch *Catch::syntaxCopy()
{
    Catch *c = new Catch(loc,
        type ? type->syntaxCopy() : NULL,
        ident,
        (handler ? handler->syntaxCopy() : NULL));
    c->internalCatch = internalCatch;
    return c;
}

bool LangPlugin::doesHandleCatch(LINK lang)
{
    return lang == LINKcpp;
}

::Catch *LangPlugin::createCatch(Loc loc, Type *t,
                        Identifier *id, Statement *handler)
{
    return new Catch(loc, t, id, handler);
}

}