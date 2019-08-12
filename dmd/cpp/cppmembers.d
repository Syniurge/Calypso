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

    override void visit(TemplateInstance tempinst)
    {
        if (tempinst.symtab)
            return;

        assert(tempinst.langPlugin().isForeignInstance(tempinst));

        assert(tempinst.tempdecl && tempinst.tempdecl.isTemplateDeclaration());
        TemplateDeclaration tempdecl = tempinst.tempdecl.isTemplateDeclaration();

        // Get the enclosing template instance from the scope tinst
        tempinst.tinst = sc.tinst;

        // Get the instantiating module from the scope minst
        tempinst.minst = sc.minst;

        assert(tempinst.semantictiargsdone);

        /* See if there is an existing TemplateInstantiation that already
        * implements the typeargs. If so, just refer to that one instead.
        */
        tempinst.inst = tempdecl.findExistingInstance(tempinst, null);
        if (!tempinst.inst)
        {
            if (auto foreignInst = tempdecl.foreignInstance(tempinst, sc))  // CALYPSO
                assert(tempinst == foreignInst);
        }
        else if (tempinst != tempinst.inst) // NOTE: for Calypso the instance might have already been created and added by DeclMapper, but the rest of addMember still needs to be done
        {
            // It's a match
            tempinst.parent = tempinst.inst.parent;
            tempinst.errors = tempinst.inst.errors;

            tempinst.tnext = tempinst.inst.tnext;
            tempinst.inst.tnext = tempinst;

            /* A module can have explicit template instance and its alias
            * in module scope (e,g, `alias Base64 = Base64Impl!('+', '/');`).
            * If the first instantiation 'inst' had happened in non-root module,
            * compiler can assume that its instantiated code would be included
            * in the separately compiled obj/lib file (e.g. phobos.lib).
            *
            * However, if 'this' second instantiation happened in root module,
            * compiler might need to invoke its codegen
            * (https://issues.dlang.org/show_bug.cgi?id=2500 & https://issues.dlang.org/show_bug.cgi?id=2644).
            * But whole import graph is not determined until all semantic pass finished,
            * so 'inst' should conservatively finish the semantic3 pass for the codegen.
            */
            if (tempinst.minst && tempinst.minst.isRoot() && !(tempinst.inst.minst && tempinst.inst.minst.isRoot()))
            {
                /* Swap the position of 'inst' and 'this' in the instantiation graph.
                * Then, the primary instance `inst` will be changed to a root instance.
                *
                * Before:
                *  non-root -> A!() -> B!()[inst] -> C!()
                *                      |
                *  root     -> D!() -> B!()[this]
                *
                * After:
                *  non-root -> A!() -> B!()[this]
                *                      |
                *  root     -> D!() -> B!()[inst] -> C!()
                */
                Module mi = tempinst.minst;
                TemplateInstance ti = tempinst.tinst;
                tempinst.minst = tempinst.inst.minst;
                tempinst.tinst = tempinst.inst.tinst;
                tempinst.inst.minst = mi;
                tempinst.inst.tinst = ti;

                if (tempinst.minst) // if inst was not speculative
                {
                    /* Add 'inst' once again to the root module members[], then the
                    * instance members will get codegen chances.
                    */
                    tempinst.inst.appendToModuleMember();
                }
            }
            return;
        }

        tempinst.inst = tempinst;
        assert(tempdecl.parent);
        tempinst.parent = tempinst.enclosing ? tempinst.enclosing : tempdecl.parent;

        tempinst.appendToModuleMember();

        TemplateInstance tempdecl_instance_idx = tempdecl.addInstance(tempinst);

        // Store the place we added it to in target_symbol_list(_idx) so we can
        // remove it later if we encounter an error.
//         Dsymbols* target_symbol_list = tempinst.appendToModuleMember();
//         size_t target_symbol_list_idx = target_symbol_list ? target_symbol_list.dim - 1 : 0;

        // Copy the syntax trees from the TemplateDeclaration
        tempinst.members = tempdecl.copySyntaxTree(tempinst);

        // Create our own scope for the template parameters
        Scope* _scope = /+tempdecl._scope+/ sc;

        tempinst.argsym = new ScopeDsymbol();
        tempinst.argsym.parent = _scope.parent;
        _scope = _scope.push(tempinst.argsym);
        _scope.tinst = tempinst.isDummy ? null : tempinst;
        _scope.minst = tempinst.minst;
        //scope.stc = 0;

        // Declare each template parameter as an alias for the argument type
        Scope* paramscope = _scope.push();
        paramscope.stc = 0;
        paramscope.protection = Prot(Prot.Kind.public_); // https://issues.dlang.org/show_bug.cgi?id=14169
                                                // template parameters should be public
        tempinst.declareParameters(paramscope);
        paramscope.pop();

        // Add members of template instance to template instance symbol table
        tempinst.symtab = new DsymbolTable();
        for (size_t i = 0; i < tempinst.members.dim; i++)
        {
            Dsymbol s = (*tempinst.members)[i];
            s.addMember(_scope, tempinst);
        }

        /* See if there is only one member of template instance, and that
        * member has the same name as the template instance.
        * If so, this template instance becomes an alias for that member.
        */
        if (tempinst.members.dim)
        {
            Dsymbol s;
            if (Dsymbol.oneMembers(tempinst.members, &s, tempdecl.ident) && s)
            {
                tempinst.aliasdecl = s;
            }
        }

        Scope* sc2;
        sc2 = _scope.push(tempinst);
        sc2.parent = tempinst;
        sc2.tinst = tempinst.isDummy ? null : tempinst;
        sc2.minst = tempinst.minst;

        for (size_t i = 0; i < tempinst.members.dim; i++)
        {
            Dsymbol s = (*tempinst.members)[i];
            s.setScope(sc2);
        }

        sc2.pop();
        _scope.pop();
    }
}

