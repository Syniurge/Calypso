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
    scope v = new CppAddMemberVisitor(sc, sds);
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
        ad.Dsymbol.addMember(sc, sds);

        with (ad)
        {
            if (!members)
                return;

            symtab = new DsymbolTable();

            for (size_t i = 0; i < members.dim; i++)
            {
                Dsymbol s = (*members)[i];
                s.addMember(sc, ad);
            }

            auto sc2 = newScope(sc);

            for (size_t i = 0; i < members.dim; i++)
            {
                auto s = (*members)[i];
                s.setScope(sc2);
            }

            sc2.pop();
        }
    }

    override void visit(TemplateDeclaration tempdecl)
    {
        tempdecl.Dsymbol.addMember(sc, sds);

        foreach (ti; tempdecl.instances)
            if (!ti.parent)
            {
                ti.parent = tempdecl.parent;
                ti.appendToModuleMember();
            }
    }

    override void visit(TemplateInstance tempinst)
    {
        if (tempinst.symtab)
            return;

        assert(tempinst.langPlugin().isForeignInstance(tempinst));

        assert(tempinst.tempdecl && tempinst.tempdecl.isTemplateDeclaration());
        TemplateDeclaration tempdecl = cast(TemplateDeclaration)tempinst.tempdecl;

        // Get the enclosing template instance from the scope tinst
        tempinst.tinst = sc.tinst;

        // Get the instantiating module from the scope minst
        tempinst.minst = sc.minst;

        assert(tempinst.semantictiargsdone);
        assert(!tempdecl.findExistingInstance(tempinst, null));

        tempinst.inst = tempinst;
        tempinst.parent = tempinst.enclosing ? tempinst.enclosing : tempdecl.parent;
        // NOTE: .enclosing is non-null only if one of the template args refer to a local symbol

        tempinst.appendToModuleMember();

        TemplateInstance tempdecl_instance_idx = tempdecl.addInstance(tempinst);

        // Store the place we added it to in target_symbol_list(_idx) so we can
        // remove it later if we encounter an error.
//         Dsymbols* target_symbol_list = tempinst.appendToModuleMember();
//         size_t target_symbol_list_idx = target_symbol_list ? target_symbol_list.dim - 1 : 0;

        // Copy the syntax trees from the TemplateDeclaration
        tempinst.members = tempdecl.copySyntaxTree(tempinst);

        // Create our own scope for the template parameters
//         Scope* _scope = /+tempdecl._scope+/ sc;

//         tempinst.argsym = new ScopeDsymbol();
//         tempinst.argsym.parent = _scope.parent;
//         _scope = _scope.push(tempinst.argsym);
//         _scope.tinst = tempinst.isDummy ? null : tempinst;
//         _scope.minst = tempinst.minst;
//         //scope.stc = 0;
//
//         // Declare each template parameter as an alias for the argument type
//         Scope* paramscope = _scope.push();
//         paramscope.stc = 0;
//         paramscope.protection = Prot(Prot.Kind.public_); // https://issues.dlang.org/show_bug.cgi?id=14169
//                                                 // template parameters should be public
//         tempinst.declareParameters(paramscope);
//         paramscope.pop();

        tempinst.symtab = new DsymbolTable();

        foreach (s; *tempinst.members)
        {
            assert(!s.parent);
            s.parent = tempinst;
            s.addMember(null, tempinst);
        }
    }
}

