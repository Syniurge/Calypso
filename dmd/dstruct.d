/**
 * Compiler implementation of the
 * $(LINK2 http://www.dlang.org, D programming language).
 *
 * Copyright:   Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dstruct.d, _dstruct.d)
 * Documentation:  https://dlang.org/phobos/dmd_dstruct.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dstruct.d
 */

module dmd.dstruct;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.declaration;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.mtype;
import dmd.opover;
import dmd.semantic3;
import dmd.target;
import dmd.tokens;
import dmd.typesem;
import dmd.typinf;
import dmd.visitor;

/***************************************
 * Search sd for a member function of the form:
 *   `extern (D) string toString();`
 * Params:
 *   sd = struct declaration to search
 * Returns:
 *   FuncDeclaration of `toString()` if found, `null` if not
 */
extern (C++) FuncDeclaration search_toString(StructDeclaration sd)
{
    Dsymbol s = search_function(sd, Id.tostring);
    FuncDeclaration fd = s ? s.isFuncDeclaration() : null;
    if (fd)
    {
        __gshared TypeFunction tftostring;
        if (!tftostring)
        {
            tftostring = new TypeFunction(ParameterList(), Type.tstring, LINK.d);
            tftostring = tftostring.merge().toTypeFunction();
        }
        fd = fd.overloadExactMatch(tftostring);
    }
    return fd;
}

/***************************************
 * Request additional semantic analysis for TypeInfo generation.
 * Params:
 *      sc = context
 *      t = type that TypeInfo is being generated for
 */
extern (C++) void semanticTypeInfo(Scope* sc, Type t)
{
    if (sc)
    {
        if (!sc.func)
            return;
        if (sc.intypeof)
            return;
        if (sc.flags & (SCOPE.ctfe | SCOPE.compile))
            return;
    }

    if (!t)
        return;

    void visitVector(TypeVector t)
    {
        semanticTypeInfo(sc, t.basetype);
    }

    void visitAArray(TypeAArray t)
    {
        semanticTypeInfo(sc, t.index);
        semanticTypeInfo(sc, t.next);
    }

    void visitStruct(TypeStruct t)
    {
        //printf("semanticTypeInfo.visit(TypeStruct = %s)\n", t.toChars());
        StructDeclaration sd = t.sym;

        /* Step 1: create TypeInfoDeclaration
         */
        if (!sc) // inline may request TypeInfo.
        {
            Scope scx;
            scx._module = sd.getModule();
            getTypeInfoType(sd.loc, t, &scx);
            sd.requestTypeInfo = true;
        }
        else if (!sc.minst)
        {
            // don't yet have to generate TypeInfo instance if
            // the typeid(T) expression exists in speculative scope.
        }
        else
        {
            getTypeInfoType(sd.loc, t, sc);
            sd.requestTypeInfo = true;

            // https://issues.dlang.org/show_bug.cgi?id=15149
            // if the typeid operand type comes from a
            // result of auto function, it may be yet speculative.
            // unSpeculative(sc, sd);
        }

        /* Step 2: If the TypeInfo generation requires sd.semantic3, run it later.
         * This should be done even if typeid(T) exists in speculative scope.
         * Because it may appear later in non-speculative scope.
         */
        if (!sd.members)
            return; // opaque struct
        if (!sd.xeq && !sd.xcmp && !sd.postblit && !sd.dtor && !sd.xhash && !search_toString(sd))
            return; // none of TypeInfo-specific members

        // If the struct is in a non-root module, run semantic3 to get
        // correct symbols for the member function.
        if (sd.semanticRun >= PASS.semantic3)
        {
            // semantic3 is already done
        }
        else if (TemplateInstance ti = sd.isInstantiated())
        {
            if (ti.minst && !ti.minst.isRoot())
                Module.addDeferredSemantic3(sd);
        }
        else
        {
            if (sd.inNonRoot())
            {
                //printf("deferred sem3 for TypeInfo - sd = %s, inNonRoot = %d\n", sd.toChars(), sd.inNonRoot());
                Module.addDeferredSemantic3(sd);
            }
        }
    }

    void visitTuple(TypeTuple t)
    {
        if (t.arguments)
        {
            foreach (arg; *t.arguments)
            {
                semanticTypeInfo(sc, arg.type);
            }
        }
    }

    /* Note structural similarity of this Type walker to that in isSpeculativeType()
     */

    Type tb = t.toBasetype();
    switch (tb.ty)
    {
        case Tvector:   visitVector(tb.isTypeVector()); break;
        case Taarray:   visitAArray(tb.isTypeAArray()); break;
        case Tstruct:   visitStruct(tb.isTypeStruct()); break;
        case Ttuple:    visitTuple (tb.isTypeTuple());  break;

        case Tclass:
        case Tenum:     break;

        default:        semanticTypeInfo(sc, tb.nextOf()); break;
    }
}

enum StructFlags : int
{
    none        = 0x0,
    hasPointers = 0x1, // NB: should use noPointers as in ClassFlags
}

enum StructPOD : int
{
    no,    // struct is not POD
    yes,   // struct is POD
    fwd,   // POD not yet computed
}

/***********************************************************
 * All `struct` declarations are an instance of this.
 */
extern (C++) class StructDeclaration : AggregateDeclaration
{ // CALYPSO some of the fields were moved to AggregateDeclaration for class value support
    bool hasIdentityAssign;     // true if has identity opAssign
    bool hasIdentityEquals;     // true if has identity opEquals
    bool hasNoFields;           // has no fields
    FuncDeclarations postblits; // Array of postblit functions
    FuncDeclaration postblit;   // aggregate postblit

    bool hasCopyCtor;       // copy constructor

    FuncDeclaration xeq;        // TypeInfo_Struct.xopEquals
    FuncDeclaration xcmp;       // TypeInfo_Struct.xopCmp
    FuncDeclaration xhash;      // TypeInfo_Struct.xtoHash
    extern (C++) __gshared FuncDeclaration xerreq;   // object.xopEquals
    extern (C++) __gshared FuncDeclaration xerrcmp;  // object.xopCmp

    StructPOD ispod;            // if struct is POD

    // For 64 bit Efl function call/return ABI
    Type arg1type;
    Type arg2type;

    // Even if struct is defined as non-root symbol, some built-in operations
    // (e.g. TypeidExp, NewExp, ArrayLiteralExp, etc) request its TypeInfo.
    // For those, today TypeInfo_Struct is generated in COMDAT.
    bool requestTypeInfo;

    extern (D) this(const ref Loc loc, Identifier id, bool inObject)
    {
        super(loc, id);
        ispod = StructPOD.fwd;
        // For forward references
        type = new TypeStruct(this);

        if (inObject)
        {
            if (id == Id.ModuleInfo && !Module.moduleinfo)
                Module.moduleinfo = this;
        }
    }

    static StructDeclaration create(Loc loc, Identifier id, bool inObject)
    {
        return new StructDeclaration(loc, id, inObject);
    }

    override Dsymbol syntaxCopy(Dsymbol s)
    {
        StructDeclaration sd =
            s ? cast(StructDeclaration)s
              : new StructDeclaration(loc, ident, false);
        return ScopeDsymbol.syntaxCopy(sd);
    }

    final void semanticTypeInfoMembers()
    {
        if (xeq &&
            xeq._scope &&
            xeq.semanticRun < PASS.semantic3done)
        {
            uint errors = global.startGagging();
            xeq.semantic3(xeq._scope);
            if (global.endGagging(errors))
                xeq = xerreq;
        }

        if (xcmp &&
            xcmp._scope &&
            xcmp.semanticRun < PASS.semantic3done)
        {
            uint errors = global.startGagging();
            xcmp.semantic3(xcmp._scope);
            if (global.endGagging(errors))
                xcmp = xerrcmp;
        }

        FuncDeclaration ftostr = search_toString(this);
        if (ftostr &&
            ftostr._scope &&
            ftostr.semanticRun < PASS.semantic3done)
        {
            ftostr.semantic3(ftostr._scope);
        }

        if (xhash &&
            xhash._scope &&
            xhash.semanticRun < PASS.semantic3done)
        {
            xhash.semantic3(xhash._scope);
        }

        if (postblit &&
            postblit._scope &&
            postblit.semanticRun < PASS.semantic3done)
        {
            postblit.semantic3(postblit._scope);
        }

        if (dtor &&
            dtor._scope &&
            dtor.semanticRun < PASS.semantic3done)
        {
            dtor.semantic3(dtor._scope);
        }
    }

    override Dsymbol search(const ref Loc loc, Identifier ident, int flags = SearchLocalsOnly)
    {
        //printf("%s.StructDeclaration::search('%s', flags = x%x)\n", toChars(), ident.toChars(), flags);
        if (_scope && !symtab)
            dsymbolSemantic(this, _scope);

        if (!members || !symtab) // opaque or semantic() is not yet called
        {
            error("is forward referenced when looking for `%s`", ident.toChars());
            return null;
        }

        return ScopeDsymbol.search(loc, ident, flags);
    }

    override const(char)* kind() const
    {
        return "struct";
    }

    override final void finalizeSize()
    {
        //printf("StructDeclaration::finalizeSize() %s, sizeok = %d\n", toChars(), sizeok);
        assert(sizeok != Sizeok.done);

        if (sizeok == Sizeok.inProcess)
        {
            return;
        }
        sizeok = Sizeok.inProcess;

        //printf("+StructDeclaration::finalizeSize() %s, fields.dim = %d, sizeok = %d\n", toChars(), fields.dim, sizeok);

        fields.setDim(0);   // workaround

        // Set the offsets of the fields and determine the size of the struct
        uint offset = 0;
        bool isunion = isUnionDeclaration() !is null;
        for (size_t i = 0; i < members.dim; i++)
        {
            Dsymbol s = (*members)[i];
            s.setFieldOffset(this, &offset, isunion);
        }
        if (type.ty == Terror)
        {
            errors = true;
            return;
        }

        // 0 sized struct's are set to 1 byte
        if (structsize == 0)
        {
            hasNoFields = true;
            structsize = 1;
            alignsize = 1;
        }

        // Round struct size up to next alignsize boundary.
        // This will ensure that arrays of structs will get their internals
        // aligned properly.
        if (alignment == STRUCTALIGN_DEFAULT)
            structsize = (structsize + alignsize - 1) & ~(alignsize - 1);
        else
            structsize = (structsize + alignment - 1) & ~(alignment - 1);

        sizeok = Sizeok.done;

        //printf("-StructDeclaration::finalizeSize() %s, fields.dim = %d, structsize = %d\n", toChars(), fields.dim, structsize);

        if (errors)
            return;

        // Calculate fields[i].overlapped
        if (checkOverlappedFields())
        {
            errors = true;
            return;
        }

        // Determine if struct is all zeros or not
        zeroInit = true;
        foreach (vd; fields)
        {
            if (vd._init)
            {
                if (vd._init.isVoidInitializer())
                    /* Treat as 0 for the purposes of putting the initializer
                     * in the BSS segment, or doing a mass set to 0
                     */
                    continue;

                // Zero size fields are zero initialized
                if (vd.type.size(vd.loc) == 0)
                    continue;

                // Examine init to see if it is all 0s.
                auto exp = vd.getConstInitializer();
                if (!exp || !_isZeroInit(exp))
                {
                    zeroInit = false;
                    break;
                }
            }
            else if (!vd.type.isZeroInit(loc))
            {
                zeroInit = false;
                break;
            }
        }

        auto tt = target.toArgTypes(type);
        size_t dim = tt ? tt.arguments.dim : 0;
        if (dim >= 1)
        {
            assert(dim <= 2);
            arg1type = (*tt.arguments)[0].type;
            if (dim == 2)
                arg2type = (*tt.arguments)[1].type;
        }
    }

    /***************************************
     * Determine if struct is POD (Plain Old Data).
     *
     * POD is defined as:
     *      $(OL
     *      $(LI not nested)
     *      $(LI no postblits, destructors, or assignment operators)
     *      $(LI no `ref` fields or fields that are themselves non-POD)
     *      )
     * The idea being these are compatible with C structs.
     *
     * Returns:
     *     true if struct is POD
     */
    final bool isPOD()
    {
        // If we've already determined whether this struct is POD.
        if (ispod != StructPOD.fwd)
            return (ispod == StructPOD.yes);

        ispod = StructPOD.yes;

        if (enclosing || postblit || dtor || hasCopyCtor)
            ispod = StructPOD.no;

        // Recursively check all fields are POD.
        for (size_t i = 0; i < fields.dim; i++)
        {
            VarDeclaration v = fields[i];
            if (v.storage_class & STC.ref_)
            {
                ispod = StructPOD.no;
                break;
            }

            Type tv = v.type.baseElemOf();
            if (tv.ty == Tstruct)
            {
                TypeStruct ts = cast(TypeStruct)tv;
                StructDeclaration sd = ts.sym;
                if (!sd.isPOD())
                {
                    ispod = StructPOD.no;
                    break;
                }
            }
        }

        return (ispod == StructPOD.yes);
    }

    bool disableDefaultCtor() // CALYPSO
    {
        return true;
    }

    override Expression defaultInit(Loc loc) // CALYPSO
    {
        Declaration d = new SymbolDeclaration(this.loc, this);
        assert(d);
        d.type = type;
        d.storage_class |= STC.rvalue; // https://issues.dlang.org/show_bug.cgi?id=14398
        return new VarExp(this.loc, d);
    }

    override final inout(StructDeclaration) isStructDeclaration() inout pure // CALYPSO needed for findFieldIndexByName assert
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**********************************
 * Determine if exp is all binary zeros.
 * Params:
 *      exp = expression to check
 * Returns:
 *      true if it's all binary 0
 */
private bool _isZeroInit(Expression exp)
{
    switch (exp.op)
    {
        case TOK.int64:
            return exp.toInteger() == 0;

        case TOK.null_:
        case TOK.false_:
            return true;

        case TOK.structLiteral:
        {
            auto sle = cast(StructLiteralExp) exp;
            foreach (i; 0 .. sle.sd.fields.dim)
            {
                auto field = sle.sd.fields[i];
                if (field.type.size(field.loc))
                {
                    auto e = (*sle.elements)[i];
                    if (e ? !_isZeroInit(e)
                          : !field.type.isZeroInit(field.loc))
                        return false;
                }
            }
            return true;
        }

        case TOK.arrayLiteral:
        {
            auto ale = cast(ArrayLiteralExp)exp;

            const dim = ale.elements ? ale.elements.dim : 0;

            if (ale.type.toBasetype().ty == Tarray) // if initializing a dynamic array
                return dim == 0;

            foreach (i; 0 .. dim)
            {
                if (!_isZeroInit(ale.getElement(i)))
                    return false;
            }

            /* Note that true is returned for all T[0]
             */
            return true;
        }

        case TOK.string_:
        {
            StringExp se = cast(StringExp)exp;

            if (se.type.toBasetype().ty == Tarray) // if initializing a dynamic array
                return se.len == 0;

            foreach (i; 0 .. se.len)
            {
                if (se.getCodeUnit(i))
                    return false;
            }
            return true;
        }

        case TOK.vector:
        {
            auto ve = cast(VectorExp) exp;
            return _isZeroInit(ve.e1);
        }

        case TOK.float64:
        case TOK.complex80:
        {
            import dmd.root.ctfloat : CTFloat;
            return (exp.toReal()      is CTFloat.zero) &&
                   (exp.toImaginary() is CTFloat.zero);
        }

        default:
            return false;
    }
}

/***********************************************************
 * Unions are a variation on structs.
 */
extern (C++) class UnionDeclaration : StructDeclaration // CALYPSO (made non final)
{
    extern (D) this(const ref Loc loc, Identifier id)
    {
        super(loc, id, false);
    }

    override Dsymbol syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto ud = new UnionDeclaration(loc, ident);
        return StructDeclaration.syntaxCopy(ud);
    }

    override const(char)* kind() const
    {
        return "union";
    }

    override inout(UnionDeclaration) isUnionDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

// CALYPSO
extern(C++) StructDeclaration isStructDeclarationOrNull(Dsymbol s)
{
    if (s !is null)
        return null;
    return s.isStructDeclaration();
}
