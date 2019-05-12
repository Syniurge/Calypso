// Contributed by Elie Morisse, same license DMD uses

/* NOTE: This replicates the parts of importAll/dsymbolSemantic1/2/3 that are of interest to Calypso.
 *       The duplication of code is worth it, because:
 *        - it (will) reduces the number of hooks and other changes needed in DMD
 *        - having parts of semantic done during declaration mapping, like
 *          AggregateDeclaration.type being set, enables referencing those declarations by
 *          taking the "post-semantic" type or declaration straightaway
*/

// module dmd.cpp.cppsemantic;
//
// import core.stdc.stdio;
// import core.stdc.string;
//
// import dmd.aggregate;
// import dmd.aliasthis;
// import dmd.arraytypes;
// import dmd.astcodegen;
// import dmd.attrib;
// import dmd.blockexit;
// import dmd.clone;
// import dmd.dcast;
// import dmd.dclass;
// import dmd.declaration;
// import dmd.denum;
// import dmd.dimport;
// import dmd.dinterpret;
// import dmd.dmodule;
// import dmd.dscope;
// import dmd.dstruct;
// import dmd.dsymbol;
// import dmd.dsymbolsem;
// import dmd.dtemplate;
// import dmd.dversion;
// import dmd.errors;
// import dmd.escape;
// import dmd.expression;
// import dmd.expressionsem;
// import dmd.func;
// import dmd.globals;
// import dmd.id;
// import dmd.identifier;
// import dmd.init;
// import dmd.initsem;
// import dmd.hdrgen;
// import dmd.mtype;
// import dmd.nogc;
// import dmd.nspace;
// import dmd.objc;
// import dmd.opover;
// import dmd.parse;
// import dmd.root.filename;
// import dmd.root.outbuffer;
// import dmd.root.rmem;
// import dmd.root.rootobject;
// import dmd.sideeffect;
// import dmd.statementsem;
// import dmd.staticassert;
// import dmd.tokens;
// import dmd.utf;
// import dmd.utils;
// import dmd.statement;
// import dmd.target;
// import dmd.templateparamsem;
// import dmd.typesem;
// import dmd.visitor;
//
// extern(C++) void cppSemantic(Dsymbol dsym, Scope* sc)
// {
//     scope v = new CppSemanticVisitor(sc);
//     dsym.accept(v);
// }
//
// extern(C++) final class CppSemanticVisitor : Visitor
// {
//     alias visit = Visitor.visit;
//     Scope* sc;
//     this(Scope* sc)
//     {
//         this.sc = sc;
//     }
//
//     override const(void*) _typeid() const // CALYPSO
//     {
//         return cast(void*) typeid(Semantic2Visitor);
//     }
//
//     override void visit(Dsymbol) {}
//
//     override void visit(TemplateInstance tempinst)
//     {
//         if (tempinst.semanticRun >= PASS.semantic2)
//             return;
//         tempinst.semanticRun = PASS.semantic2;
//         static if (LOG)
//         {
//             printf("+TemplateInstance.semantic2('%s')\n", tempinst.toChars());
//         }
//         if (!tempinst.errors && tempinst.members)
//         {
//             TemplateDeclaration tempdecl = tempinst.tempdecl.isTemplateDeclaration();
//             assert(tempdecl);
//
//             sc = tempdecl._scope;
//             assert(sc);
//             sc = sc.push(tempinst.argsym);
//             sc = sc.push(tempinst);
//             sc.tinst = tempinst.isDummy ? null : tempinst;
//             sc.minst = tempinst.minst;
//
//             int needGagging = (tempinst.gagged && !global.gag);
//             uint olderrors = global.errors;
//             int oldGaggedErrors = -1; // dead-store to prevent spurious warning
//             if (needGagging)
//                 oldGaggedErrors = global.startGagging();
//
//             for (size_t i = 0; i < tempinst.members.dim; i++)
//             {
//                 Dsymbol s = (*tempinst.members)[i];
//                 static if (LOG)
//                 {
//                     printf("\tmember '%s', kind = '%s'\n", s.toChars(), s.kind());
//                 }
//                 s.semantic2(sc);
//                 if (tempinst.gagged && global.errors != olderrors)
//                     break;
//             }
//
//             if (global.errors != olderrors)
//             {
//                 if (!tempinst.errors)
//                 {
//                     if (!tempdecl.literal)
//                         tempinst.error(tempinst.loc, "error instantiating");
//                     if (tempinst.tinst)
//                         tempinst.tinst.printInstantiationTrace();
//                 }
//                 tempinst.errors = true;
//             }
//             if (needGagging)
//                 global.endGagging(oldGaggedErrors);
//
//             sc = sc.pop();
//             sc.pop();
//         }
//         static if (LOG)
//         {
//             printf("-TemplateInstance.semantic2('%s')\n", tempinst.toChars());
//         }
//     }
//
//
//     override void visit(VarDeclaration vd)
//     {
//         if (vd.semanticRun < PASS.semanticdone && vd.inuse)
//             return;
//
//         //printf("VarDeclaration::semantic2('%s')\n", toChars());
//
//         if (vd.aliassym)        // if it's a tuple
//         {
//             vd.aliassym.accept(this);
//             vd.semanticRun = PASS.semantic2done;
//             return;
//         }
//
//         if (vd._init && !vd.toParent().isFuncDeclaration())
//         {
//             vd.inuse++;
//             // https://issues.dlang.org/show_bug.cgi?id=14166
//             // Don't run CTFE for the temporary variables inside typeof
//             vd._init = vd._init.initializerSemantic(sc, vd.type, sc.intypeof == 1 ? INITnointerpret : INITinterpret);
//             vd.inuse--;
//         }
//         if (vd._init && vd.storage_class & STC.manifest)
//         {
//             /* Cannot initializer enums with CTFE classreferences and addresses of struct literals.
//              * Scan initializer looking for them. Issue error if found.
//              */
//             if (ExpInitializer ei = vd._init.isExpInitializer())
//             {
//                 static bool hasInvalidEnumInitializer(Expression e)
//                 {
//                     static bool arrayHasInvalidEnumInitializer(Expressions* elems)
//                     {
//                         foreach (e; *elems)
//                         {
//                             if (e && hasInvalidEnumInitializer(e))
//                                 return true;
//                         }
//                         return false;
//                     }
//
//                     if (e.op == TOK.classReference)
//                         return true;
//                     if (e.op == TOK.address && (cast(AddrExp)e).e1.op == TOK.structLiteral)
//                         return true;
//                     if (e.op == TOK.arrayLiteral)
//                         return arrayHasInvalidEnumInitializer((cast(ArrayLiteralExp)e).elements);
//                     if (e.op == TOK.structLiteral)
//                         return arrayHasInvalidEnumInitializer((cast(StructLiteralExp)e).elements);
//                     if (e.op == TOK.assocArrayLiteral)
//                     {
//                         AssocArrayLiteralExp ae = cast(AssocArrayLiteralExp)e;
//                         return arrayHasInvalidEnumInitializer(ae.values) ||
//                                arrayHasInvalidEnumInitializer(ae.keys);
//                     }
//                     return false;
//                 }
//
//                 if (hasInvalidEnumInitializer(ei.exp))
//                     vd.error(": Unable to initialize enum with class or pointer to struct. Use static const variable instead.");
//             }
//         }
//         else if (vd._init && vd.isThreadlocal())
//         {
//             // Cannot initialize a thread-local class or pointer to struct variable with a literal
//             // that itself is a thread-local reference and would need dynamic initialization also.
//             if ((vd.type.ty == Tclass) && vd.type.isMutable() && !vd.type.isShared())
//             {
//                 ExpInitializer ei = vd._init.isExpInitializer();
//                 if (ei && ei.exp.op == TOK.classReference)
//                     vd.error("is a thread-local class and cannot have a static initializer. Use `static this()` to initialize instead.");
//             }
//             else if (vd.type.ty == Tpointer && vd.type.nextOf().ty == Tstruct && vd.type.nextOf().isMutable() && !vd.type.nextOf().isShared())
//             {
//                 ExpInitializer ei = vd._init.isExpInitializer();
//                 if (ei && ei.exp.op == TOK.address && (cast(AddrExp)ei.exp).e1.op == TOK.structLiteral)
//                     vd.error("is a thread-local pointer to struct and cannot have a static initializer. Use `static this()` to initialize instead.");
//             }
//         }
//         vd.semanticRun = PASS.semantic2done;
//     }
//
//     override void visit(Module mod)
//     {
//         if (mod.semanticRun >= PASS.semantic3done)
//             return;
//         Scope* sc = Scope.createGlobal(mod); // create root scope
//         for (size_t i = 0; i < mod.members.dim; i++)
//         {
//             Dsymbol s = (*mod.members)[i];
//             s.cppSemantic(sc);
//         }
//         sc = sc.pop();
//         sc.pop();
//         mod.semanticRun = PASS.semantic3done;
//     }
//
//     override void visit(FuncDeclaration fd)
//     {
//         import dmd.dmangle : mangleToFuncSignature;
//
//         if (fd.semanticRun >= PASS.semantic2done)
//             return;
//         assert(fd.semanticRun <= PASS.semantic2);
//         fd.semanticRun = PASS.semantic2;
//
//         //printf("FuncDeclaration::semantic2 [%s] fd0 = %s %s\n", loc.toChars(), toChars(), type.toChars());
//
//         // https://issues.dlang.org/show_bug.cgi?id=18385
//         // Disable for 2.079, s.t. a deprecation cycle can be started with 2.080
//         if (0)
//         if (fd.overnext && !fd.errors)
//         {
//             OutBuffer buf1;
//             OutBuffer buf2;
//
//             // Always starts the lookup from 'this', because the conflicts with
//             // previous overloads are already reported.
//             auto f1 = fd;
//             mangleToFuncSignature(buf1, f1);
//
//             overloadApply(f1, (Dsymbol s)
//             {
//                 auto f2 = s.isFuncDeclaration();
//                 if (!f2 || f1 == f2 || f2.errors)
//                     return 0;
//
//                 // Don't have to check conflict between declaration and definition.
//                 if ((f1.fbody !is null) != (f2.fbody !is null))
//                     return 0;
//
//                 /* Check for overload merging with base class member functions.
//                  *
//                  *  class B { void foo() {} }
//                  *  class D : B {
//                  *    override void foo() {}    // B.foo appears as f2
//                  *    alias foo = B.foo;
//                  *  }
//                  */
//                 if (f1.overrides(f2))
//                     return 0;
//
//                 // extern (C) functions always conflict each other.
//                 if (f1.ident == f2.ident &&
//                     f1.toParent2() == f2.toParent2() &&
//                     (f1.linkage != LINK.d && f1.linkage != LINK.cpp) &&
//                     (f2.linkage != LINK.d && f2.linkage != LINK.cpp))
//                 {
//                     /* Allow the hack that is actually used in druntime,
//                      * to ignore function attributes for extern (C) functions.
//                      * TODO: Must be reconsidered in the future.
//                      *  BUG: https://issues.dlang.org/show_bug.cgi?id=18206
//                      *
//                      *  extern(C):
//                      *  alias sigfn_t  = void function(int);
//                      *  alias sigfn_t2 = void function(int) nothrow @nogc;
//                      *  sigfn_t  bsd_signal(int sig, sigfn_t  func);
//                      *  sigfn_t2 bsd_signal(int sig, sigfn_t2 func) nothrow @nogc;  // no error
//                      */
//                     if (f1.fbody is null || f2.fbody is null)
//                         return 0;
//
//                     auto tf1 = cast(TypeFunction)f1.type;
//                     auto tf2 = cast(TypeFunction)f2.type;
//                     error(f2.loc, "%s `%s%s` cannot be overloaded with %s`extern(%s)` function at %s",
//                             f2.kind(),
//                             f2.toPrettyChars(),
//                             parametersTypeToChars(tf2.parameters, tf2.varargs),
//                             (f1.linkage == f2.linkage ? "another " : "").ptr,
//                             linkageToChars(f1.linkage), f1.loc.toChars());
//                     f2.type = Type.terror;
//                     f2.errors = true;
//                     return 0;
//                 }
//
//                 buf2.reset();
//                 mangleToFuncSignature(buf2, f2);
//
//                 auto s1 = buf1.peekString();
//                 auto s2 = buf2.peekString();
//
//                 //printf("+%s\n\ts1 = %s\n\ts2 = %s @ [%s]\n", toChars(), s1, s2, f2.loc.toChars());
//                 if (strcmp(s1, s2) == 0)
//                 {
//                     auto tf2 = cast(TypeFunction)f2.type;
//                     error(f2.loc, "%s `%s%s` conflicts with previous declaration at %s",
//                             f2.kind(),
//                             f2.toPrettyChars(),
//                             parametersTypeToChars(tf2.parameters, tf2.varargs),
//                             f1.loc.toChars());
//                     f2.type = Type.terror;
//                     f2.errors = true;
//                 }
//                 return 0;
//             });
//         }
//         objc.setSelector(fd, sc);
//         objc.validateSelector(fd);
//         if (ClassDeclaration cd = fd.parent.isClassDeclaration())
//         {
//             objc.checkLinkage(fd);
//         }
//         if (!fd.type || fd.type.ty != Tfunction)
//             return;
//         TypeFunction f = cast(TypeFunction) fd.type;
//         if (!f.parameters)
//             return;
//         size_t nparams = Parameter.dim(f.parameters);
//         //semantic for parameters' UDAs
//         foreach (i; 0..nparams)
//         {
//             Parameter param = Parameter.getNth(f.parameters, i);
//             if (param && param.userAttribDecl)
//                 param.userAttribDecl.semantic2(sc);
//         }
//     }
//
//     override void visit(Import i)
//     {
//         // do nothing?
//     }
//
//     override void visit(AttribDeclaration ad)
//     {
//         Dsymbols* d = ad.include(sc);
//         if (d)
//         {
//             Scope* sc2 = ad.newScope(sc);
//             for (size_t i = 0; i < d.dim; i++)
//             {
//                 Dsymbol s = (*d)[i];
//                 s.cppSemantic(sc2);
//             }
//             if (sc2 != sc)
//                 sc2.pop();
//         }
//     }
//
//     override void visit(AggregateDeclaration ad)
//     {
//         if (!ad.members)
//             return;
//
//         auto sc2 = ad.newScope(sc);
//
//         ad.determineSize(ad.loc);
//
//         for (size_t i = 0; i < ad.members.dim; i++)
//         {
//             Dsymbol s = (*ad.members)[i];
//             s.cppSemantic(sc2);
//         }
//
//         sc2.pop();
//
//         ad.semanticRun = PASS.semantic3done;
//     }
//
//     override void visit(StructDeclaration sd)
//     {
//         if (sd.semanticRun >= PASS.semantic3done)
//             return;
//
// //         Scope* scx = null;
//
//         sd.type = sd.type.typeSemantic(sd.loc, sc);
//         if (sd.type.ty == Tstruct && (cast(TypeStruct)sd.type).sym != sd)
//         {
//             auto ti = (cast(TypeStruct)sd.type).sym.isInstantiated();
//             if (ti && isError(ti))
//                 (cast(TypeStruct)sd.type).sym = sd;
//         }
//
//         // Ungag errors when not speculative
//         Ungag ungag = sd.ungagSpeculative();
//
//         if (sd.semanticRun == PASS.init)
//         {
//             sd.protection = sc.protection;
//
//             sd.alignment = sc.alignment();
//
//             sd.storage_class |= sc.stc;
//             if (sd.storage_class & STC.deprecated_)
//                 sd.isdeprecated = true;
//             if (sd.storage_class & STC.abstract_)
//                 sd.error("structs, unions cannot be `abstract`");
//
//             sd.userAttribDecl = sc.userAttribDecl;
//
//             if (sc.linkage == LINK.cpp)
//                 sd.classKind = ClassKind.cpp;
//         }
//         else if (sd.symtab && !scx)
//             return;
//
//         sd.semanticRun = PASS.semantic;
//
//         if (!sd.members) // if opaque declaration
//         {
//             sd.semanticRun = PASS.semanticdone;
//             return;
//         }
//         if (!sd.symtab)
//         {
//             sd.symtab = new DsymbolTable();
//
//             for (size_t i = 0; i < sd.members.dim; i++)
//             {
//                 auto s = (*sd.members)[i];
//                 //printf("adding member '%s' to '%s'\n", s.toChars(), this.toChars());
//                 s.addMember(sc, sd);
//             }
//         }
//
//         auto sc2 = sd.newScope(sc);
//
//         /* Set scope so if there are forward references, we still might be able to
//          * resolve individual members like enums.
//          */
//         for (size_t i = 0; i < sd.members.dim; i++)
//         {
//             auto s = (*sd.members)[i];
//             //printf("struct: setScope %s %s\n", s.kind(), s.toChars());
//             s.setScope(sc2);
//         }
//
//         for (size_t i = 0; i < sd.members.dim; i++)
//         {
//             auto s = (*sd.members)[i];
//             s.importAll(sc2);
//         }
//
//         for (size_t i = 0; i < sd.members.dim; i++)
//         {
//             auto s = (*sd.members)[i];
//             s.dsymbolSemantic(sc2);
//             sd.errors |= s.errors;
//         }
//         if (sd.errors)
//             sd.type = Type.terror;
//
//         if (!sd.determineFields())
//         {
//             if (sd.type.ty != Terror)
//             {
//                 sd.error(sd.loc, "circular or forward reference");
//                 sd.errors = true;
//                 sd.type = Type.terror;
//             }
//
//             sc2.pop();
//             sd.semanticRun = PASS.semanticdone;
//             return;
//         }
//         /* Following special member functions creation needs semantic analysis
//          * completion of sub-structs in each field types. For example, buildDtor
//          * needs to check existence of elaborate dtor in type of each fields.
//          * See the case in compilable/test14838.d
//          */
//         foreach (v; sd.fields)
//         {
//             Type tb = v.type.baseElemOf();
//             if (tb.ty != Tstruct)
//                 continue;
//             auto sdec = (cast(TypeStruct)tb).sym;
//             if (sdec.semanticRun >= PASS.semanticdone)
//                 continue;
//
//             sc2.pop();
//
//             sd._scope = scx ? scx : sc.copy();
//             sd._scope.setNoFree();
//             sd._scope._module.addDeferredSemantic(sd);
//             sd.semanticRun = PASS.semantic;
//             //printf("\tdeferring %s\n", toChars());
//             return;
//         }
//
//         /* Look for special member functions.
//          */
//         sd.aggNew = cast(NewDeclaration)sd.search(Loc.initial, Id.classNew);
//         sd.aggDelete = cast(DeleteDeclaration)sd.search(Loc.initial, Id.classDelete);
//
//         // Look for the constructor
//         sd.ctor = sd.searchCtor();
//
//         sd.dtor = buildDtor(sd, sc2);
//         sd.tidtor = buildExternDDtor(sd, sc2);
//         sd.postblit = buildPostBlit(sd, sc2);
//
//         buildOpAssign(sd, sc2);
//         buildOpEquals(sd, sc2);
//
//         if (global.params.useTypeInfo && Type.dtypeinfo)  // these functions are used for TypeInfo
//         {
//             sd.xeq = buildXopEquals(sd, sc2);
//             sd.xcmp = buildXopCmp(sd, sc2);
//             sd.xhash = buildXtoHash(sd, sc2);
//         }
//
//         sd.inv = buildInv(sd, sc2);
//
//         Module.dprogress++;
//         sd.semanticRun = PASS.semanticdone;
//         //printf("-StructDeclaration::semantic(this=%p, '%s')\n", sd, sd.toChars());
//
//         sc2.pop();
//
//         if (sd.ctor)
//         {
//             Dsymbol scall = sd.search(Loc.initial, Id.call);
//             if (scall)
//             {
//                 uint xerrors = global.startGagging();
//                 sc = sc.push();
//                 sc.tinst = null;
//                 sc.minst = null;
//                 auto fcall = resolveFuncCall(sd.loc, sc, scall, null, null, null, 1);
//                 sc = sc.pop();
//                 global.endGagging(xerrors);
//
//                 if (fcall && fcall.isStatic())
//                 {
//                     sd.error(fcall.loc, "`static opCall` is hidden by constructors and can never be called");
//                     errorSupplemental(fcall.loc, "Please use a factory method instead, or replace all constructors with `static opCall`.");
//                 }
//             }
//         }
//
//         if (sd.type.ty == Tstruct && (cast(TypeStruct)sd.type).sym != sd)
//         {
//             // https://issues.dlang.org/show_bug.cgi?id=19024
//             StructDeclaration sym = (cast(TypeStruct)sd.type).sym;
//             version (none)
//             {
//                 printf("this = %p %s\n", sd, sd.toChars());
//                 printf("type = %d sym = %p, %s\n", sd.type.ty, sym, sym.toPrettyChars());
//             }
//             sd.error("already exists at %s. Perhaps in another function with the same name?", sym.loc.toChars());
//         }
//
//         if (global.errors != errors)
//         {
//             // The type is no good.
//             sd.type = Type.terror;
//             sd.errors = true;
//             if (sd.deferred)
//                 sd.deferred.errors = true;
//         }
//
//         if (sd.deferred && !global.gag)
//         {
//             sd.deferred.semantic2(sc);
//             sd.deferred.semantic3(sc);
//         }
//
//         if (!sd.langPlugin()) // CALYPSO
//             markAggregateReferenced(sd);
//     }
// }
