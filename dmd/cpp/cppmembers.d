// Contributed by Elie Morisse, same license DMD uses

/* Do not wait for semantic to determine symtab and _scope
*/

module dmd.cpp.cppmembers;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.attrib;
import dmd.blockexit;
import dmd.clone;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.dversion;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.initsem;
import dmd.hdrgen;
import dmd.mtype;
import dmd.nogc;
import dmd.nspace;
import dmd.objc;
import dmd.opover;
import dmd.parse;
import dmd.root.filename;
import dmd.root.outbuffer;
import dmd.root.rmem;
import dmd.root.rootobject;
import dmd.sideeffect;
import dmd.statementsem;
import dmd.staticassert;
import dmd.tokens;
import dmd.utf;
import dmd.utils;
import dmd.semantic2;
import dmd.semantic3;
import dmd.statement;
import dmd.target;
import dmd.templateparamsem;
import dmd.typesem;
import dmd.visitor;

extern(C++) void cppAddMember(Dsymbol dsym, Scope* sc, ScopeDsymbol sds)
{
    scope v = new CppAddMemberVisitor(sc);
    dsym.accept(v);
}

extern(C++) final class CppAddMemberVisitor : Visitor
{
    alias visit = Visitor.visit;

    Scope* sc;
    ScopeDsymbol sds;
    this(Scope* sc, ScopeDsymbol sds)
    {
        this.sc = sc;
        this.sds = sds;
    }

    override void visit(Dsymbol dsym)
    {
    }

    override void visit(AggregateDeclaration ad)
    {
        with (ad)
        {
            addMember(sc, sds);

            if (!members)
                return;

            symtab = new DsymbolTable();

            for (size_t i = 0; i < members.dim; i++)
            {
                Dsymbol s = (*members)[i];
                s.addMember(sc, ad);
            }

            auto sc2 = newScope(sc);

            for (size_t i = 0; i < sd.members.dim; i++)
            {
                auto s = (*members)[i];
                s.setScope(sc2);
            }

            sc2.pop();
        }
    }

    override void visit(TemplateInstance tempinst)
    {
        with (tempinst)
        {
            addMember(sc, sds);

            if (!members)
                return;

            symtab = new DsymbolTable();

            for (size_t i = 0; i < members.dim; i++)
            {
                Dsymbol s = (*members)[i];
                s.addMember(sc, ad);
            }

            auto sc2 = newScope(sc);

            for (size_t i = 0; i < sd.members.dim; i++)
            {
                auto s = (*members)[i];
                s.setScope(sc2);
            }

            sc2.pop();
        }
    }
}

