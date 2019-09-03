// Contributed by Elie Morisse, same license DMD uses

/* NOTE: This replicates the parts of importAll/dsymbolSemantic1/2/3 that are of interest to Calypso.
 *       The duplication of code is worth it, because:
 *        - it (will) reduces the number of hooks and other changes needed in DMD
 *        - having parts of semantic done during declaration mapping, like
 *          AggregateDeclaration.type being set, enables referencing those declarations by
 *          taking the "post-semantic" type or declaration straightaway
*/

module dmd.cpp.cppsemantic;

import core.stdc.stdio;
import core.stdc.string;

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

extern(C++) void cppSemantic(Dsymbol dsym, Scope* sc)
{
    scope v = new CppSemanticVisitor(sc);
    dsym.accept(v);
}

extern(C++) final class CppSemanticVisitor : DsymbolSemanticVisitor
{
    alias visit = DsymbolSemanticVisitor.visit;

    this(Scope* sc)
    {
        super(sc);
    }

    override void visit(TemplateInstance tempinst)
    {
        if (tempinst.semanticRun >= PASS.semantic3done)
            return;

        tempinst.addMember(sc, sc.scopesym);

        // Give additional context info if error occurred during instantiation
//         if (global.errors != errorsave)
//         {
//             if (!tempinst.errors)
//             {
//                 if (!tempdecl.literal)
//                     tempinst.error(tempinst.loc, "error instantiating");
//                 if (tempinst.tinst)
//                     tempinst.tinst.printInstantiationTrace();
//             }
//             tempinst.errors = true;
//         }

        tempinst.semanticRun = PASS.semantic3done;
    }

    override void visit(StructDeclaration sd)
    {
        if (sd.semanticRun >= PASS.semantic3done)
            return;

        Scope* sc = sd._scope ? sd._scope : this.sc;
        assert(sc);
        sd._scope = null;

        sd.type = sd.type.typeSemantic(sd.loc, sc);

        sd.protection = sc.protection;
        sd.alignment = sc.alignment();
        sd.storage_class |= sc.stc;

        sd.semanticRun = PASS.semantic;

        if (!sd.members) // if opaque declaration
        {
            sd.semanticRun = PASS.semantic3done;
            return;
        }
        if (!sd.symtab)
        {
            sd.symtab = new DsymbolTable();

            for (size_t i = 0; i < sd.members.dim; i++)
            {
                auto s = (*sd.members)[i];
                s.addMember(sc, sd);
            }
        }

        auto sc2 = sd.newScope(sc);

        /* Set scope so if there are forward references, we still might be able to
         * resolve individual members like enums.
         */
        for (size_t i = 0; i < sd.members.dim; i++)
        {
            auto s = (*sd.members)[i];
            s.setScope(sc2);
        }

        for (size_t i = 0; i < sd.members.dim; i++)
        {
            auto s = (*sd.members)[i];
            s.dsymbolSemantic(sc2);
        }

        if (!sd.determineFields())
        {
            sd.error(sd.loc, "circular or forward reference");
            sd.errors = true;
            sd.type = Type.terror;

            sc2.pop();
            sd.semanticRun = PASS.semantic3done;
            return;
        }
        /* Following special member functions creation needs semantic analysis
         * completion of sub-structs in each field types. For example, buildDtor
         * needs to check existence of elaborate dtor in type of each fields.
         * See the case in compilable/test14838.d
         */
        foreach (v; sd.fields)
        {
            Type tb = v.type.baseElemOf();
            if (!tb.isAggregateValue())
                continue;
            auto ad = tb.getAggregateSym();
            if (ad.semanticRun < PASS.semanticdone)
                continue;
            ad.dsymbolSemantic(null);
            assert(ad.semanticRun >= PASS.semantic3done);
        }

        /* Look for special member functions.
         */
        sd.aggNew = cast(NewDeclaration)sd.search(Loc.initial, Id.classNew);
        sd.aggDelete = cast(DeleteDeclaration)sd.search(Loc.initial, Id.classDelete);

        // Look for the constructor
        sd.ctor = sd.searchCtor();

        sd.dtor = buildDtor(sd, sc2);
        sd.tidtor = sd.dtor; // CALYPSO
        sd.postblit = buildPostBlit(sd, sc2);

        buildOpAssign(sd, sc2);
        buildOpEquals(sd, sc2);

        if (global.params.useTypeInfo && Type.dtypeinfo)  // these functions are used for TypeInfo
        {
            sd.xeq = buildXopEquals(sd, sc2);
            sd.xcmp = buildXopCmp(sd, sc2);
            sd.xhash = buildXtoHash(sd, sc2);
        }

        sd.determineSize(sd.loc);

        if (!sd.getRTInfo)
        {
            // Evaluate: RTinfo!type
            auto tiargs = new Objects();
            tiargs.push(sd.type);
            auto ti = new TemplateInstance(sd.loc, Type.rtinfo, tiargs);

            Scope* sc3 = ti.tempdecl._scope.startCTFE();
            sc3.tinst = sc.tinst;
            sc3.minst = sc.minst;

            ti.dsymbolSemantic(sc3);
            ti.semantic2(sc3);
            ti.semantic3(sc3);
            auto e = resolve(Loc.initial, sc3, ti.toAlias(), false);

            sc3.endCTFE();

            e = e.ctfeInterpret();
            sd.getRTInfo = e;
        }
        sd.semanticTypeInfoMembers();

//         if (sd.langPlugin.isSymbolReferenced(sd))
//             MarkAggregateReferencedImpl(sd);

        sd.semanticRun = PASS.semantic3done;

        sc2.pop();
    }

    override void visit(UnionDeclaration ud)
    {
        visit(cast(StructDeclaration)ud);
    }

    override void visit(ClassDeclaration cldec)
    {
        if (cldec.semanticRun >= PASS.semanticdone)
            return;
        int errors = global.errors;

        Scope* sc = cldec._scope ? cldec._scope : this.sc;
        assert(sc);
        cldec._scope = null;

        cldec.type = cldec.type.typeSemantic(cldec.loc, sc);

        cldec.protection = sc.protection;

        cldec.alignment = sc.alignment(); // CALYPSO (class values)

        cldec.storage_class |= sc.stc;
        if (cldec.storage_class & STC.scope_)
            cldec.stack = true;
        if (cldec.storage_class & STC.abstract_)
            cldec.isabstract = Abstract.yes;

        cldec.semanticRun = PASS.semantic;

        assert(cldec.baseok < Baseok.done);
        cldec.baseok = Baseok.start;

        // See if there's a base class as first in baseclasses[]
        if (cldec.baseclasses.dim)
        {
            BaseClass* b = (*cldec.baseclasses)[0];
            Type tb = b.type.toBasetype();
            auto sym = getAggregateSym(tb); // CALYPSO
            auto bcd = isClassDeclarationOrNull(sym);

            assert(!sym.isInterfaceDeclaration());

            /* https://issues.dlang.org/show_bug.cgi?id=11034
                * Class inheritance hierarchy
                * and instance size of each classes are orthogonal information.
                * Therefore, even if tc.sym.sizeof == Sizeok.none,
                * we need to set baseClass field for class covariance check.
                */
            cldec.baseClass = sym;
            b.sym = cldec.baseClass;
        }

        for (size_t i = (cldec.baseClass ? 1 : 0); i < cldec.baseclasses.dim; i++)
        {
            BaseClass* b = (*cldec.baseclasses)[i];
            Type tb = b.type.toBasetype();
            b.sym = getAggregateSym(tb);
        }
        cldec.baseok = Baseok.done;

        if (cldec.baseClass)
        {
            cldec.enclosing = cldec.baseClass.enclosing;
            cldec.storage_class |= cldec.baseClass.storage_class & STC.TYPECTOR;
        }

        cldec.interfaces = cldec.baseclasses.tdata()[(cldec.baseClass ? 1 : 0) .. cldec.baseclasses.dim];

        if (!cldec.members) // if opaque declaration
        {
            cldec.semanticRun = PASS.semantic3done;
            return;
        }
        if (!cldec.symtab)
        {
            cldec.symtab = new DsymbolTable();

            /* https://issues.dlang.org/show_bug.cgi?id=12152
             * The semantic analysis of base classes should be finished
             * before the members semantic analysis of this class, in order to determine
             * vtbl in this class. However if a base class refers the member of this class,
             * it can be resolved as a normal forward reference.
             * Call addMember() and setScope() to make this class members visible from the base classes.
             */
            for (size_t i = 0; i < cldec.members.dim; i++)
            {
                auto s = (*cldec.members)[i];
                s.addMember(sc, cldec);
            }

            auto sc2 = cldec.newScope(sc);

            /* Set scope so if there are forward references, we still might be able to
             * resolve individual members like enums.
             */
            for (size_t i = 0; i < cldec.members.dim; i++)
            {
                auto s = (*cldec.members)[i];
                s.setScope(sc2);
            }

            sc2.pop();
        }

        cldec.baseok = Baseok.semanticdone;
        cldec.initVtbl(); // CALYPSO

        auto sc2 = cldec.newScope(sc);

        for (size_t i = 0; i < cldec.members.dim; ++i)
        {
            auto s = (*cldec.members)[i];
            s.importAll(sc2);
        }

        // Note that members.dim can grow due to tuple expansion during semantic()
        for (size_t i = 0; i < cldec.members.dim; ++i)
        {
            auto s = (*cldec.members)[i];
            s.dsymbolSemantic(sc2);
        }

        if (!cldec.determineFields())
        {
            assert(cldec.type == Type.terror);
            sc2.pop();
            return;
        }
        cldec.finalizeVtbl(); // CALYPSO
        /* Following special member functions creation needs semantic analysis
         * completion of sub-structs in each field types.
         */
        foreach (v; cldec.fields)
        {
            Type tb = v.type.baseElemOf();
            if (!tb.isAggregateValue())
                continue;
            auto ad = tb.getAggregateSym();
            if (ad.semanticRun < PASS.semanticdone)
                continue;
            ad.dsymbolSemantic(null);
            assert(ad.semanticRun >= PASS.semantic3done);
        }

        /* Look for special member functions.
         * They must be in this class, not in a base class.
         */
        // Can be in base class
        cldec.aggNew = cast(NewDeclaration)cldec.search(Loc.initial, Id.classNew);
        cldec.aggDelete = cast(DeleteDeclaration)cldec.search(Loc.initial, Id.classDelete);

        // Look for the constructor
        cldec.ctor = cldec.searchCtor();

        cldec.dtor = buildDtor(cldec, sc2);
        cldec.tidtor = buildExternDDtor(cldec, sc2);

//         if (cldec.langPlugin.isSymbolReferenced(cldec))
//             MarkAggregateReferencedImpl(cldec);

        cldec.semanticRun = PASS.semantic3done;

        sc2.pop();
    }

    override void visit(VarDeclaration dsym)
    {
        if (dsym.semanticRun >= PASS.semantic3done)
            return;

        Scope* sc = dsym._scope ? dsym._scope : this.sc;
        assert(sc);
        dsym._scope = null;

        dsym.semanticRun = PASS.semantic;

        /* Pick up storage classes from context, but except synchronized,
         * override, abstract, and final.
         */
        dsym.storage_class |= (sc.stc & ~(STC.synchronized_ | STC.override_ | STC.abstract_ | STC.final_));

        AggregateDeclaration ad = dsym.isThis();
        if (ad)
            dsym.storage_class |= ad.storage_class & STC.TYPECTOR;

        assert(dsym.type);

        dsym.linkage = sc.linkage;
        dsym.parent = sc.parent;
        dsym.protection = sc.protection;

        /* If scope's alignment is the default, use the type's alignment,
         * otherwise the scope overrrides.
         */
        dsym.alignment = sc.alignment();
        if (dsym.alignment == STRUCTALIGN_DEFAULT)
            dsym.alignment = dsym.type.alignment(); // use type's alignment

        Dsymbol parent = dsym.toParent();

        Type tb = dsym.type.toBasetype();
        Type tbn = tb.baseElemOf();

        /* Storage class can modify the type
         */
        dsym.type = dsym.type.addStorageClass(dsym.storage_class);

        /* Adjust storage class to reflect type
         */
        if (dsym.type.isConst())
        {
            dsym.storage_class |= STC.const_;
            if (dsym.type.isShared())
                dsym.storage_class |= STC.shared_;
        }
        else if (dsym.type.isImmutable())
            dsym.storage_class |= STC.immutable_;

        if (dsym.storage_class & (STC.static_ | STC.extern_ | STC.manifest | STC.templateparameter | STC.tls | STC.gshared | STC.ctfe))
        {
        }
        else
        {
            AggregateDeclaration aad = parent.isAggregateDeclaration();
            if (aad)
            {
                if (global.params.vfield && dsym.storage_class & (STC.const_ | STC.immutable_) && dsym._init && !dsym._init.isVoidInitializer())
                {
                    const(char)* s = (dsym.storage_class & STC.immutable_) ? "immutable" : "const";
                    message(dsym.loc, "`%s.%s` is `%s` field", ad.toPrettyChars(), dsym.toChars(), s);
                }
                dsym.storage_class |= STC.field;
                if (tbn.ty == Tstruct && (cast(TypeStruct)tbn).sym.noDefaultCtor) // CALYPSO TODO: think harder about noDefaultCtor
                {
                    if (!dsym.isThisDeclaration() && !dsym._init)
                        aad.noDefaultCtor = true;
                }
            }
        }

        if (!(dsym.storage_class & (STC.ctfe | STC.ref_ | STC.result)) && tbn.ty == Tstruct && (cast(TypeStruct)tbn).sym.noDefaultCtor)
        {
            if (!dsym._init)
            {
                if (dsym.isField())
                {
                    /* For fields, we'll check the constructor later to make sure it is initialized
                     */
                    dsym.storage_class |= STC.nodefaultctor;
                }
            }
        }

        FuncDeclaration fd = parent.isFuncDeclaration();

        if ((!dsym._init || dsym._init.isVoidInitializer) && !fd)
        {
            // If not mutable, initializable by constructor only
            dsym.storage_class |= STC.ctorinit;
        }

        if (dsym._init)
        {
            dsym.storage_class |= STC.init; // remember we had an explicit initializer

            ExpInitializer ei = dsym._init.isExpInitializer();
            if (ei) // https://issues.dlang.org/show_bug.cgi?id=13424
                    // Preset the required type to fail in FuncLiteralDeclaration::semantic3
                ei.exp = inferType(ei.exp, dsym.type);

            dsym._init = dsym._init.initializerSemantic(sc, dsym.type, sc.intypeof == 1 ? INITnointerpret : INITinterpret);
        }

        dsym.semanticRun = PASS.semantic3done;

        if (sc.scopesym && !sc.scopesym.isAggregateDeclaration())
        {
            for (ScopeDsymbol sym = sc.scopesym; sym && dsym.endlinnum == 0;
                 sym = sym.parent ? sym.parent.isScopeDsymbol() : null)
                dsym.endlinnum = sym.endlinnum;
        }
    }

    override void visit(FuncDeclaration funcdecl)
    {
        AggregateDeclaration ad;

        if (funcdecl.semanticRun >= PASS.semantic3done)
            return;
        assert(funcdecl.semanticRun <= PASS.semantic);
        funcdecl.semanticRun = PASS.semantic;

        sc = funcdecl._scope ? funcdecl._scope : this.sc;
        assert(sc);
        funcdecl._scope = null;

        funcdecl.parent = sc.parent;
        Dsymbol parent = funcdecl.toParent();

        funcdecl.foverrides.setDim(0); // reset in case semantic() is being retried for this function

        funcdecl.storage_class |= sc.stc & ~STC.ref_;
        ad = funcdecl.isThis();
        if (sc.func)
            funcdecl.storage_class |= sc.func.storage_class & STC.disable;
        // Remove prefix storage classes silently.
        if ((funcdecl.storage_class & STC.TYPECTOR) && !(ad || funcdecl.isNested()))
            funcdecl.storage_class &= ~STC.TYPECTOR;

        FuncLiteralDeclaration fld = funcdecl.isFuncLiteralDeclaration();
        if (fld && fld.treq)
        {
            Type treq = fld.treq;
            assert(treq.nextOf().ty == Tfunction);
            if (treq.ty == Tdelegate)
                fld.tok = TOK.delegate_;
            else if (treq.ty == Tpointer && treq.nextOf().ty == Tfunction)
                fld.tok = TOK.function_;
            else
                assert(0);
            funcdecl.linkage = treq.nextOf().toTypeFunction().linkage;
        }
        else
            funcdecl.linkage = sc.linkage;
        funcdecl.inlining = sc.inlining;
        funcdecl.protection = sc.protection;
        funcdecl.userAttribDecl = sc.userAttribDecl;

        if (!funcdecl.originalType)
            funcdecl.originalType = funcdecl.type.syntaxCopy();
        if (!funcdecl.type.deco)
        {
            sc = sc.push();
            sc.stc |= funcdecl.storage_class & (STC.disable | STC.deprecated_); // forward to function type

            TypeFunction tf = funcdecl.type.toTypeFunction();

            if (tf.isref)
                sc.stc |= STC.ref_;
            if (tf.isscope)
                sc.stc |= STC.scope_;
            if (tf.isnothrow)
                sc.stc |= STC.nothrow_;
            if (tf.isnogc)
                sc.stc |= STC.nogc;

            if (funcdecl.isCtorDeclaration())
            {
                sc.flags |= SCOPE.ctor;
                Type tret = ad.handleType();
                assert(tret);
                tret = tret.addStorageClass(funcdecl.storage_class | sc.stc);
                tret = tret.addMod(funcdecl.type.mod);
                tf.next = tret;
                sc.stc |= STC.ref_; // CALYPSO-specific
            }

            sc.linkage = funcdecl.linkage;

            /* Apply const, immutable, wild and shared storage class
             * to the function type. Do this before type semantic.
             */
            auto stc = funcdecl.storage_class;
            if (funcdecl.type.isImmutable())
                stc |= STC.immutable_;
            if (funcdecl.type.isConst())
                stc |= STC.const_;
            if (funcdecl.type.isShared() || funcdecl.storage_class & STC.synchronized_)
                stc |= STC.shared_;
            if (funcdecl.type.isWild())
                stc |= STC.wild;
            funcdecl.type = funcdecl.type.addSTC(stc);

            funcdecl.type = funcdecl.type.typeSemantic(funcdecl.loc, sc);
            sc = sc.pop();
        }

        if (auto ctd = funcdecl.isCtorDeclaration())
        {
            TypeFunction tf = ctd.type.toTypeFunction();

            /* See if it's the default constructor
            */
            if (Parameter.dim(tf.parameters) == 0 && tf.varargs == 0)
                ad.defaultCtor = ctd;
        }

        // Merge back function attributes into 'originalType'.
        // It's used for mangling, ddoc, and json output.
        TypeFunction tfo = funcdecl.originalType.toTypeFunction();
        TypeFunction tfx = funcdecl.type.toTypeFunction();
        tfo.mod = tfx.mod;
        tfo.isscope = tfx.isscope;
        tfo.isscopeinferred = tfx.isscopeinferred;
        tfo.isref = tfx.isref;
        tfo.isnothrow = tfx.isnothrow;
        tfo.isnogc = tfx.isnogc;
        tfo.isproperty = tfx.isproperty;
        tfo.purity = tfx.purity;
        tfo.trust = tfx.trust;

        funcdecl.storage_class &= ~(STC.TYPECTOR | STC.FUNCATTR);

        /* Do not allow template instances to add virtual functions
         * to a class.
         */
        if (funcdecl.isVirtual())
        {
            TemplateInstance ti = parent.isTemplateInstance();
            if (ti)
            {
                // Take care of nested templates
                while (1)
                {
                    TemplateInstance ti2 = ti.tempdecl.parent.isTemplateInstance();
                    if (!ti2)
                        break;
                    ti = ti2;
                }

                // If it's a member template
                ClassDeclaration cd = ti.tempdecl.isClassMember();
                if (cd)
                {
                    funcdecl.error("cannot use template to add virtual function to class `%s`", cd.toChars());
                }
            }
        }

        Module.dprogress++; // CALYPSO TODO shouldn't be needed, remove
        funcdecl.semanticRun = PASS.semanticdone;

        /* Save scope for possible later use (if we need the
         * function internals)
         */
        funcdecl._scope = sc.copy(); // CALYPSO TODO consider removing?
        funcdecl._scope.setNoFree();

        assert(funcdecl.type.ty != Terror || funcdecl.errors);

        funcdecl.semantic3(sc);
    }

    override void visit(CtorDeclaration ctd)
    {
        visit(cast(FuncDeclaration)ctd);
    }

    override void visit(DtorDeclaration dd)
    {
        if (!dd.type)
            dd.type = new TypeFunction(null, Type.tvoid, false, LINK.d, dd.storage_class);
        visit(cast(FuncDeclaration)dd);
    }

    override void visit(EnumDeclaration ed)
    {
        if (ed.semanticRun >= PASS.semanticdone)
            return;
        assert(ed.semanticRun == PASS.init);

        Scope* sc = ed._scope ? ed._scope : this.sc;
        assert(sc);
        ed._scope = null;

        ed.parent = sc.parent;
        ed.type = ed.type.typeSemantic(ed.loc, sc);

        ed.protection = sc.protection;

        ed.semanticRun = PASS.semantic;

        if (!ed.members && !ed.memtype) // enum ident;
        {
            ed.semanticRun = PASS.semanticdone;
            return;
        }

        if (!ed.symtab)
            ed.symtab = new DsymbolTable();

        if (ed.memtype)
            ed.memtype = ed.memtype.typeSemantic(ed.loc, sc);

        ed.semanticRun = PASS.semanticdone;

        if (!ed.members) // enum ident : memtype;
        {
            assert(ed.memtype);
            if (!ed.defaultval)
                ed.defaultval = defaultInit(ed.memtype, ed.loc); // CALYPSO-specific: C++ enums may be empty, and EnumDeclaration.getDefaultValue() errors if both defaultval and members are null
            return;
        }

        Module.dprogress++;

        Scope* sce;
        if (ed.isAnonymous())
            sce = sc;
        else
        {
            sce = sc.push(ed);
            sce.parent = ed;
        }
        sce = sce.startCTFE();
        sce.setNoFree(); // needed for getMaxMinValue()

        /* Each enum member gets the sce scope
         */
        for (size_t i = 0; i < ed.members.dim; i++)
        {
            EnumMember em = (*ed.members)[i].isEnumMember();
            if (em)
                em._scope = sce;
        }

        if (!ed.added) // CALYPSO FIXME this shouldn't be needed, enum members should have their scope set during mapping
        {
            /* addMember() is not called when the EnumDeclaration appears as a function statement,
             * so we have to do what addMember() does and install the enum members in the right symbol
             * table
             */
            ScopeDsymbol scopesym = null;
            if (ed.isAnonymous())
            {
                /* Anonymous enum members get added to enclosing scope.
                 */
                for (Scope* sct = sce; 1; sct = sct.enclosing)
                {
                    assert(sct);
                    if (sct.scopesym)
                    {
                        scopesym = sct.scopesym;
                        if (!sct.scopesym.symtab)
                            sct.scopesym.symtab = new DsymbolTable();
                        break;
                    }
                }
            }
            else
            {
                // Otherwise enum members are in the EnumDeclaration's symbol table
                scopesym = ed;
            }

            for (size_t i = 0; i < ed.members.dim; i++)
            {
                EnumMember em = (*ed.members)[i].isEnumMember();
                if (em)
                {
                    em.ed = ed;
                    em.addMember(sc, scopesym);
                }
            }
        }

        for (size_t i = 0; i < ed.members.dim; i++)
        {
            EnumMember em = (*ed.members)[i].isEnumMember();
            if (em)
                em.dsymbolSemantic(em._scope);
        }
    }
}
