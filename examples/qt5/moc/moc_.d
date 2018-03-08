/**
 * D replacement of Qt's MOC (Meta-Object Compiler).
 *
 * Contributed by Elie Morisse.
 */

module moc.moc_;

pragma (cppmap, "<QtCore>");
pragma (cppmap, "<private/qmetaobject_p.h>");

import cpp.core;
import std.traits : isCallable, hasUDA;

import (C++)
    QArrayData,
    Qt.ConnectionType;

public import
    cpp.memberptr,
    moc.types;

public import (C++)
    QObject,
    QMetaObject,
    QMetaObjectPrivate,
    MetaDataFlags,
    MethodFlags,
    QBasicAtomicInteger,
    _ : qRegisterMetaType;

public alias QByteArrayData = QArrayData;

enum QT_NO_DEBUG = true;

///////////////////////////////////////////////////////////////////////////////

enum signals;
enum slots;

///////////////////////////////////////////////////////////////////////////////

enum isSignal(alias F) = is(typeof(F) == function) && hasUDA!(F, signals);
enum isSlot(alias F) = is(typeof(F) == function) && hasUDA!(F, slots);
enum isConstructor(alias F) = is(typeof(F) == function) && __traits(identifier, F) == "__ctor";
template isMethod(alias F)
{
    private enum ident = __traits(identifier, F);
    static if (ident == "qt_metacast" || ident == "qt_metacall" || ident == "metaObject"
                    || ident == "__ctor" || ident == "__dtor")
        enum isMethod = false;
    else
        enum isMethod = is(typeof(F) == function) && !__traits(isStaticFunction, F) && !isSignal!F && !isSlot!F;
}

public alias symalias(alias S) = S;

///////////////////////////////////////////////////////////////////////////////

// WARNING: the order of mixins matters (BUG?), signals have to be declared before mixin Q_OBJECT.

// The actual signal generation is deferred in order to generate indices
mixin template signal(string name, T)
    if ( isCallable!T )
{
    mixin ("@signals struct _signal_" ~ name ~ " {\n" ~
        " enum _name = \"" ~ name ~ "\";\n" ~
        " alias _T = " ~ T.stringof ~ "; \n}");
}

///////////////////////////////////////////////////////////////////////////////

mixin template Q_OBJECT()
{
private:
    import cpp.traits : MostDerivedCppClass;

public:
    struct ClassDef
    {
        import
            std.algorithm,
            std.conv,
            std.format,
            std.traits,
            std.typetuple;

        alias C = symalias!(__traits(parent, typeof(this)));

        alias signalSeedList = Filter!(_isSignalSeed, __traits(derivedMembers, C));

        /*********/

        //
        // Generate internal signal functions
        //
        static string getSignalStr(string name, T)()
        {
            string result = "extern(C++)  @signals final " ~ ReturnType!T.stringof ~ " " ~ name ~ "(";
            string argArrayElems;

            uint i = 0;
            foreach(t; ParameterTypeTuple!T)
            {
                if (i)
                    result ~= ", ";
                auto i_str = to!string(i++);
                result ~= t.stringof ~ " _t" ~ i_str;
                argArrayElems ~= ", cast(void*) &_t" ~ i_str;
            }

            result ~= ") {\n" ~
                "    void*[] _a = [ null" ~ argArrayElems ~ " ];\n" ~
                    "    QMetaObject.activate(this, &staticMetaObject, _signal_" ~ name ~ "_index, _a.ptr);\n" ~
                "}\n";
            return result;
        }

        static string genSignals()
        {
            uint idx = 0;

            string result = "private {\n";
            foreach(m; signalSeedList)
            {
                alias signalSeed = symalias!(__traits(getMember, C, m));
                result ~= "  enum _signal_" ~ signalSeed._name ~ "_index = " ~ idx++.to!string ~ ";\n";
            }
            result ~= "}\n\n";

            foreach(m; signalSeedList)
            {
                alias signalSeed = symalias!(__traits(getMember, C, m));
                result ~= getSignalStr!(signalSeed._name, signalSeed._T) ~ "\n";
            }
            return result;
        }

        /*********/

    //     struct Interface
    //     {
    //         string className;
    //         string interfaceId;
    //     };
    //     QList<QList<Interface> >interfaceList;

        enum hasQObject = true;
        enum hasQGadget = false;

    //     struct PluginData {
    //         string iid;
    //         QMap<QString, QJsonArray> metaArgs;
    //         QJsonDocument metaData;
    //     } pluginData;

        alias constructorList = Filter!(_isConstructor, __traits(derivedMembers, C));
        alias signalList = staticMap!(SeedToSignal, signalSeedList);
        alias slotList = Filter!(_isSlot, __traits(derivedMembers, C));
        alias methodList = Filter!(_isMethod, __traits(derivedMembers, C));
        enum int notifyableProperties = 0;
        enum string[] propertyList = [];
    //     QList<ClassInfoDef> classInfoList;
    //     QMap<string, bool> enumDeclarations;
        enum string[] enumList = [];
    //     QMap<string, string> flagAliases;
        enum int revisionedMethods = 0;
        enum int revisionedProperties = 0;
    //
    //     int begin;
    //     int end;

        /*********/

        enum _isSignal(string m) = isSignal!(symalias!(__traits(getMember, C, m)));
        enum _isSlot(string m) = isSlot!(symalias!(__traits(getMember, C, m)));
        enum _isMethod(string m) = isMethod!(symalias!(__traits(getMember, C, m)));
        enum _isConstructor(string m) = isConstructor!(symalias!(__traits(getMember, C, m)));

        template _isSignalSeed(string m)
        {
            alias s = symalias!(__traits(getMember, C, m));

            static if (!is(s == struct))
                enum _isSignalSeed = false;
            else
                enum _isSignalSeed = hasUDA!((__traits(getMember, C, m)), signals);
        }

        enum SeedToSignal(string m) = __traits(getMember, C, m)._name;

        /*********/

        static string[] getStrings()
        {
            string[] result;

            void strreg(in string s) {
                if (!result.canFind(s))
                    result ~= s.dup;
            }

        //
        // Register all strings used in data section
        //
            strreg(fullyQualifiedName!C);
//              registerClassInfoStrings();

            void registerFunctionStrings(list...)()
            {
                foreach (s; list) {
                    strreg(s);
                    foreach (f; __traits(getOverloads, C, s)) {
                        alias RetType = ReturnType!(typeof(f));
                        static if (isConstructor!f)
                            strreg("");
                        else static if (!isQBuiltinType!RetType)
                            strreg(DtoCXXType!RetType);
//                          strreg(f.tag);

                        foreach(ArgType; ParameterTypeTuple!f)
                            static if (!isQBuiltinType!ArgType)
                                strreg(DtoCXXType!ArgType);

                        foreach(ArgName; ParameterIdentifierTuple!f)
                            strreg(ArgName);
                    }
                }
            }

            registerFunctionStrings!signalList();
            registerFunctionStrings!slotList();
            registerFunctionStrings!methodList();
            registerFunctionStrings!constructorList();
//             registerPropertyStrings();
//             registerEnumStrings();

            return result;
        }

        static string QT_MOC_LITERAL(ulong idx, ulong ofs, ulong len)
        {
            // TODO replace first arg by Q_REFCOUNT_INITIALIZE_STATIC?
            static if (__traits(hasMember, QBasicAtomicInteger!int, "__ctor"))
                string s = "{ { QBasicAtomicInteger!int(-1) }, ";
            else
                string s = "{ { { -1 } }, ";
            return s ~ len.to!string ~ ", 0, 0, " ~
                "cast(ptrdiff_t) (qt_meta_stringdata_t.stringdata0.offsetof + " ~ ofs.to!string ~
                " - " ~ idx.to!string ~ " * QByteArrayData.sizeof)" ~ "}";
        }

        static string genMetaStringData()
        {
            auto strings = getStrings();
            immutable constCharArraySizeLimit = 65535;
            string result;

        //
        // Build stringdata struct
        //
            result ~= "struct qt_meta_stringdata_t {\n";
            result ~= format("    QByteArrayData[%d] data;\n", strings.length);
            {
                ulong stringDataLength = 0;
                ulong stringDataCounter = 0;
                for (int i = 0; i < strings.length; ++i) {
                    auto thisLength = strings[i].length + 1;
                    stringDataLength += thisLength;
                    if (stringDataLength / constCharArraySizeLimit) {
                        // save previous stringdata and start computing the next one.
                        result ~= format("    char[%d] stringdata%d;\n", stringDataLength - thisLength, stringDataCounter++);
                        stringDataLength = thisLength;
                    }
                }
                result ~= format("    char[%d] stringdata%d;\n", stringDataLength, stringDataCounter);

            }
            result ~= "};\n";

            result ~= "static immutable qt_meta_stringdata_t qt_meta_stringdata = {\n";
            result ~= "    [\n";
            {
                int idx = 0;
                for (int i = 0; i < strings.length; ++i) {
                    auto str = strings[i];
                    result ~= QT_MOC_LITERAL(i, idx, str.length);
                    if (i != strings.length - 1)
                        result ~= ",";
        //             const QByteArray comment = str.length > 32 ? str.left(29) + "..." : str;
                    result ~= format(" // \"%s\"\n", str);
                    idx += str.length + 1;
        //             for (int j = 0; j < str.length; ++j) {
        //                 if (str[j] == '\\') {
        //                     int cnt = lengthOfEscapeSequence(str, j) - 1;
        //                     idx -= cnt;
        //                     j += cnt;
        //                 }
        //             }
                }
            }
            result ~= "\n    ],\n";

        //
        // Build stringdata array
        //
            result ~= "    \"";
            int col = 0;
            ulong len = 0;
            ulong stringDataLength = 0;
            for (int i = 0; i < strings.length; ++i) {
                auto s = strings[i];
                len = s.length;
                stringDataLength += len + 1;
                if (stringDataLength >= constCharArraySizeLimit) {
                    result ~= "\",\n    \"";
                    stringDataLength = len + 1;
                    col = 0;
                } else if (i)
                    result ~= "\\0"; // add \0 at the end of each string

                if (col && col + len >= 72) {
                    result ~= "\"\n    \"";
                    col = 0;
                } else if (len && s[0] >= '0' && s[0] <= '9') {
                    result ~= "\"\"";
                    len += 2;
                }
                int idx = 0;
                while (idx < s.length) {
                    if (idx > 0) {
                        col = 0;
                        result ~= "\"\n    \"";
                    }
                    int spanLen = min(70, s.length - idx);
        //             // don't cut escape sequences at the end of a line
        //             int backSlashPos = s.lastIndexOf('\\', idx + spanLen - 1);
        //             if (backSlashPos >= idx) {
        //                 int escapeLen = lengthOfEscapeSequence(s, backSlashPos);
        //                 spanLen = min(max(spanLen, backSlashPos + escapeLen - idx), s.length - idx);
        //             }
                    result ~= format("%.*s", spanLen, s[idx..idx + spanLen]);
                    idx += spanLen;
                    col += spanLen;
                }
                col += len + 2;
            }

        // Terminate stringdata struct
            result ~= "\"\n};\n";
            return result;
        }

        //
        // build the data array
        //
        static string genMetaDataArray()
        {
            auto strings = getStrings();

            long stridx(in string s)
            {
//                 if (!__ctfe)
//                 {
//                     if (count(strings, s) != 1)
//                         writeln("couldn't find ", s);
//                     writeln("count(strings, ", s, ") == ", count(strings, s));
//                 }
                assert(count(strings, s) == 1);
                return countUntil(strings, s);
            }

            string result;
            enum MetaObjectPrivateFieldCount = QMetaObjectPrivate.sizeof / int.sizeof;
            int index = MetaObjectPrivateFieldCount;
            result ~= "static immutable(uint[]) qt_meta_data = [\n";
            result ~= "\n // content:\n";
            result ~= format("    %4d,       // revision\n", cast(int) QMetaObjectPrivate.OutputRevision);
            result ~= format("    %4d,       // classname\n", stridx(fullyQualifiedName!C));
            result ~= format("    %4d, %4d, // classinfo\n", 0, 0/+classInfoList.length, classInfoList.length ? index : 0+/); // FIXME
        //     index += classInfoList.length * 2;

            // Returns the sum of all parameters (including return type) for the given
            // \a list of methods. This is needed for calculating the size of the methods'
            // parameter type/name meta-data.
            int aggregateParameterCount(list...)()
            {
                int sum = 0;
                foreach (s; list) {
                    foreach (f; __traits(getOverloads, C, s)) {
                        alias Args = ParameterTypeTuple!(typeof(f));
                        sum += Args.length + 1; // +1 for return type
                    }
                }
                return sum;
            }

            int methodCount = signalList.length + slotList.length + methodList.length;
            result ~= format("    %4d, %4d, // methods\n", methodCount, methodCount ? index : 0);
            index += methodCount * 5;
        //     if (revisionedMethods)
        //         index += methodCount;
            int paramsIndex = index;
            int totalParameterCount = aggregateParameterCount!signalList()
                    + aggregateParameterCount!slotList()
                    + aggregateParameterCount!methodList()
                    + aggregateParameterCount!constructorList();
            index += totalParameterCount * 2 // types and parameter names
                    - methodCount // return "parameters" don't have names
                    - constructorList.length; // "this" parameters don't have names

            result ~= format("    %4d, %4d, // properties\n", propertyList.length, propertyList.length ? index : 0);
            index += propertyList.length * 3;
            if (notifyableProperties)
                index += propertyList.length;
            if (revisionedProperties)
                index += propertyList.length;
            result ~= format("    %4d, %4d, // enums/sets\n", enumList.length, enumList.length ? index : 0);

            bool isConstructible = constructorList.length;
            int enumsIndex = index;
//             for (int i = 0; i < enumList.length; ++i)
//                 index += 4 + (enumList[i].values.length * 2); // TODO: by "values" we mean the number of enum members
            result ~= format("    %4d, %4d, // constructors\n", isConstructible ? constructorList.length : 0,
                    isConstructible ? index : 0);

            int flags = 0;
        //     if (hasQGadget) {
        //         // Ideally, all the classes could have that flag. But this broke classes generated
        //         // by qdbusxml2cpp which generate code that require that we call qt_metacall for properties
        //         flags |= PropertyAccessInStaticMetaCall;
        //     }
            result ~= format("    %4d,       // flags\n", flags);
            result ~= format("    %4d,       // signalCount\n", signalList.length);

        //
        // Build classinfo array
        //
//             generateClassInfos();

            void genFunctions(list...)(in string functype, int type, ref int paramsIndex)
            {
                if (!list.length)
                    return;
                result ~= format("\n // %ss: name, argc, parameters, tag, flags\n", functype);

                foreach (s; list) {
                    foreach (f; __traits(getOverloads, C, s)) {
                        string comment;
                        ubyte flags = cast(ubyte) type;
                        if (__traits(getProtection, f) == "private") {
                            flags |= MethodFlags.AccessPrivate;
                            comment ~= "Private";
                        } else if (__traits(getProtection, f) == "public") {
                            flags |= MethodFlags.AccessPublic;
                            comment ~= "Public";
                        } else if (__traits(getProtection, f) == "protected") {
                            flags |= MethodFlags.AccessProtected;
                            comment ~= "Protected";
                        }
//                         if (f.isCompat) {
//                             flags |= MethodCompatibility;
//                             comment ~= " | MethodCompatibility";
//                         }
//                         if (f.wasCloned) {
//                             flags |= MethodCloned;
//                             comment ~= " | MethodCloned";
//                         }
//                         if (f.isScriptable) {
//                             flags |= MethodScriptable;
//                             comment ~= " | isScriptable";
//                         }
//                         if (f.revision > 0) {
//                             flags |= MethodRevisioned;
//                             comment ~= " | MethodRevisioned";
//                         }

                        alias Args = ParameterTypeTuple!(typeof(f));
                        result ~= format("    %4d, %4d, %4d, %4d, 0x%02x /* %s */,\n",
                            stridx(s), Args.length, paramsIndex, 0 /+FIXME stridx(f.tag)+/, flags, comment);

                        paramsIndex += 1 + Args.length * 2;
                    }
                }
            }

        //
        // Build signals array first, otherwise the signal indices would be wrong
        //
            genFunctions!signalList("signal", MethodFlags.MethodSignal, paramsIndex);

        //
        // Build slots array
        //
            genFunctions!slotList("slot", MethodFlags.MethodSlot, paramsIndex);

        //
        // Build method array
        //
            genFunctions!methodList("method", MethodFlags.MethodMethod, paramsIndex);

        //
        // Build method version arrays
        //
        //     void generateFunctionRevisions(const QList<FunctionDef>& list, const char *functype)
        //     {
        //         if (list.count())
        //             result ~= format("\n // %ss: revision\n", functype);
        //         for (int i = 0; i < list.count(); ++i) {
        //             const FunctionDef &f = list[i];
        //             result ~= format("    %4d,\n", f.revision);
        //         }
        //     }

            if (revisionedMethods) {
        //         generateFunctionRevisions(signalList, "signal");
        //         generateFunctionRevisions(slotList, "slot");
        //         generateFunctionRevisions(methodList, "method");
            }

        //
        // Build method parameters array
        //
            void genFunctionParameters(list...)(in string functype)
            {
                if (!list.length)
                    return;
                result ~= format("\n // %ss: parameters\n", functype);
                foreach (s; list) {
                    foreach (f; __traits(getOverloads, C, s)) {
                        result ~= "    ";

                        // Types
                        static if (isConstructor!f)
                            genTypeInfoByName("");
                        else
                            genTypeInfo!(ReturnType!f)();
                        result ~= ",";
                        foreach (ArgType; ParameterTypeTuple!(typeof(f)))
                        {
                            result ~= " ";
                            genTypeInfo!ArgType();
                            result ~= ",";
                        }

                        // Parameter names
                        foreach (ArgName; ParameterIdentifierTuple!f)
                            result ~= format(" %4d,", stridx(ArgName));

                        result ~= "\n";
                    }
                }
            }

            void genTypeInfo(T)()
            {
                static if (isQBuiltinType!T) {
                    static if (is(T == qreal)) {
                        enum int type = cast(int) QMetaType.Type.UnknownType;
                        enum valueString = "QReal";
                    } else {
                        enum int type = nameToQBuiltinType!(T.stringof);
                        enum valueString = metaTypeEnumValueString!type;
                    }
                    static if (valueString.length) {
                        result ~= format("QMetaType.Type.%s", valueString);
                    } else {
                        assert(type != QMetaType.Type.UnknownType);
                        result ~= format("%4d", type);
                    }
                } else {
                    result ~= format("0x%.8x | %d", MetaDataFlags.IsUnresolvedType, stridx(T.stringof));
                }
            }

            void genTypeInfoByName()(string typeName)
            {
                result ~= format("0x%.8x | %d", MetaDataFlags.IsUnresolvedType, stridx(typeName));
            }

            genFunctionParameters!signalList("signal");
            genFunctionParameters!slotList("slot");
            genFunctionParameters!methodList("method");
            if (isConstructible)
                genFunctionParameters!constructorList("constructor");

        //
        // Build property array
        //
        //     generateProperties();

        //
        // Build enums array
        //
        //     generateEnums(enumsIndex);

        //
        // Build constructors array
        //
            if (isConstructible)
                genFunctions!constructorList("constructor", MethodFlags.MethodConstructor, paramsIndex);

        //
        // Terminate data array
        //
            result ~= "\n       0        // eod\n];\n\n";
            return result;
        }

        //
        // Generate internal qt_static_metacall() function
        //
        static string genStaticMetaCallBody(bool isQGadget = false)()
        {
            string result;

            bool needElse = false;
            bool isUsed_a = false;

//             if (constructorList.length) {
//                 result ~= "    if (_c == QMetaObject.Call.CreateInstance) {\n";
//                 result ~= "        switch (_id) {\n";
//                 for (int ctorindex = 0; ctorindex < c_constructorList.length; ++ctorindex) {
//                     result ~= format("        case %d: { %s *_r = new %s(", ctorindex,
//                             __traits(identifier, C), __traits(identifier, C));
//                     const FunctionDef &f = constructorList[ctorindex];
//                     int offset = 1;
//
//                     int argsCount = f.arguments.length;
//                     for (int j = 0; j < argsCount; ++j) {
//                         const ArgumentDef &a = f.arguments[j];
//                         if (j)
//                             result ~= ",";
//                         result ~= format("(*cast( %s) (_a[%d]))", a.typeNameForCast.constData(), offset++);
//                     }
//                     if (f.isPrivateSignal) {
//                         if (argsCount > 0)
//                             result ~= ", ";
//                         result ~= "QPrivateSignal()";
//                     }
//                     result ~= ");\n";
//                     result ~= format("            if (_a[0]) *cast(%s**) _a[0] = _r; } break;\n",
//                             isQGadget ? "void" : "QObject");
//                 }
//                 result ~= "        default: break;\n";
//                 result ~= "        }\n";
//                 result ~= "    }";
//                 needElse = true;
//                 isUsed_a = true;
//             }

            enum hasInvokables = signalList.length || slotList.length || methodList.length;

            static if (hasInvokables) {
                if (needElse)
                    result ~= " else ";
                else
                    result ~= "    ";
                result ~= "if (_c == QMetaObject.Call.InvokeMetaMethod) {\n";
                static if (hasQObject && !QT_NO_DEBUG)
                    result ~= "        assert(staticMetaObject.cast(_o));\n"; // FIXME identifier taken
                result ~= format("        auto _t = cast(%s)(_o);\n", __traits(identifier, C));
                result ~= "        final switch (_id) {\n";

                int methodindex = 0;

                void addMethods(list...)()
                {
                    foreach (s; list) {
                        foreach (f; __traits(getOverloads, C, s)) {
                            alias fty = typeof(f);
                            result ~= format("        case %d: ", methodindex++);
                            static if (!is(ReturnType!fty == void))
                                result ~= format("{ %s _r = ", ReturnType!fty.stringof);
                            result ~= "_t.";
//                             if (f.inPrivateClass.size())
//                                 result ~= format("%s.", f.inPrivateClass.constData());
                            result ~= format("%s(", s);
                            int offset = 1;

                            alias Args = ParameterTypeTuple!fty;
                            bool needComma = false;
                            foreach (a; Args) {
                                if (needComma)
                                    result ~= ",";
                                needComma = true;
                                result ~= format("*cast(%s*) (_a[%d])", a.stringof, offset++);
                                isUsed_a = true;
                            }
                            static if (isSignal!f && __traits(getProtection, f) == "private") {
                                static if (Args.length)
                                    result ~= ", ";
                                result ~= format("%s", "QPrivateSignal()");
                            }
                            result ~= ");";
                            static if (!is(ReturnType!fty == void)) {
                                result ~= format("\n            if (_a[0]) *cast( %s*) (_a[0]) = _r; } ",
                                        ReturnType!fty.stringof);
                                isUsed_a = true;
                            }
                            result ~= " break;\n";
                        }
                    }
                }

                addMethods!signalList();
                addMethods!slotList();
                addMethods!methodList();

//                 result ~= "        default: ;\n";
                result ~= "        }\n";
                result ~= "    }";
                needElse = true;

//                 uint[][string][int] methodsWithAutomaticTypes; // original type = QMap<int, QMultiMap<QByteArray, int> >

                bool hasMethodWithAutomaticType = false;
                int idx2 = 0;
                void methodsWithAutomaticTypesHelper(list...)()
                {
                    foreach (s; list) {
                        foreach (f; __traits(getOverloads, C, s)) {
                            bool foundAutoType = false;
                            int argumentID = 0;
                            foreach (Arg; ParameterTypeTuple!(typeof(f))) {
                                static if (registerableMetaType!Arg && !isQBuiltinType!Arg) {
                                    if (!hasMethodWithAutomaticType) {
                                        result ~= " else if (_c == QMetaObject.Call.RegisterMethodArgumentMetaType) {\n";
                                        result ~= "        switch (_id) {\n";
                                        result ~= "          default: *cast(int*) _a[0] = -1; break;\n";
                                        hasMethodWithAutomaticType = true;
                                    }

                                    if (!foundAutoType) {
                                        result ~= format("        case %d:\n", idx2);
                                        result ~= "            switch (*cast(int*) _a[1]) {\n";
                                        result ~= "              default: *cast(int*) _a[0] = -1; break;\n";
                                    }

                                    foundAutoType = true;
                                    result ~= format("              case %d:\n", argumentID);
                                    result ~= format("                *cast(int*) _a[0] = qRegisterMetaType!(%s)(); break;\n", Arg.stringof); // WARNING: this is horrible if the same type is used a lot of time, but CTFE rejects my byKeyValue() call..

//                                     if (idx2 !in methodsWithAutomaticTypes || Arg.stringof !in methodsWithAutomaticTypes[idx2])
//                                         methodsWithAutomaticTypes[idx2][Arg.stringof] = [];
//                                     methodsWithAutomaticTypes[idx2][Arg.stringof] ~= j;
                                }

                                argumentID++;
                            }

                            if (foundAutoType) {
                                result ~= "            }\n";
                                result ~= "            break;\n";
                            }

                            idx2++;
                        }
                    }
                }

                methodsWithAutomaticTypesHelper!signalList();
                methodsWithAutomaticTypesHelper!slotList();
                methodsWithAutomaticTypesHelper!methodList();

                if (hasMethodWithAutomaticType) {
                    result ~= "        }\n";
                    result ~= "    }";
                    isUsed_a = true;
                }

//                 if (methodsWithAutomaticTypes.length) {
//                     result ~= " else if (_c == QMetaObject.Call.RegisterMethodArgumentMetaType) {\n";
//                     result ~= "        switch (_id) {\n";
//                     result ~= "        default: *cast(int*) _a[0] = -1; break;\n";
//                     foreach (ref it; methodsWithAutomaticTypes.byKeyValue) {
//                         result ~= format("        case %d:\n", it.key);
//                         result ~= "            switch (*cast(int*) _a[1]) {\n";
//                         result ~= "            default: *cast(int*) _a[0] = -1; break;\n";
//                         foreach (ref argit; it.value.byKeyValue) {
//                             foreach (ref argumentID; argit.value)
//                                 result ~= format("            case %d:\n", argumentID);
//                             result ~= format("                *cast(int*) _a[0] = qRegisterMetaType!(%s)(); break;\n", argit.key);
//                         }
//                         result ~= "            }\n";
//                         result ~= "            break;\n";
//                     }
//                     result ~= "        }\n";
//                     result ~= "    }";
//                     isUsed_a = true;
//                 }

            }
            static if (signalList.length) {
                assert(needElse); // if there is signal, there was method.
                result ~= " else if (_c == QMetaObject.Call.IndexOfMethod) {\n";
                result ~= "        int *result = cast(int *) (_a[0]);\n";
                result ~= "        void **func = cast(void **) (_a[1]);\n";
                bool anythingUsed = false;
                int signalindex = 0;
                foreach (s; signalList) {
                    foreach (f; __traits(getOverloads, C, s)) {
                        static if (/+f.wasCloned || !f.inPrivateClass.isEmpty() || +/__traits(isStaticFunction, f))
                            continue;
                        anythingUsed = true;
                        result ~= "        {\n";
//                         result ~= "            alias _t = CppMemberFuncPtr;\n";

//                         alias Args = ParameterTypeTuple!(typeof(f));
//                         for (int j = 0; j < Args.length; ++j) {
//                             alias a = Args[j];
//                             if (j)
//                                 result ~= ", ";
//                             result ~= format("%s", typeid(a)/+QByteArray(a.type.name + ' ' + a.rightType).constData()+/);
//                         }
//                         static if ( __traits(getProtection, f) == "private") {
//                             if (Args.length > 0)
//                                 result ~= ", ";
//                             result ~= format("%s", "QPrivateSignal");
//                         }
//                         static if (isConst!F)
//                             result ~= ") const;\n";
//                         else
//                             result ~= ");\n";
                        auto cppmfp = format("cpp_member_ptr!(%s, %s.%s)",
                                    __traits(identifier, C), __traits(identifier, C), __traits(identifier, f));
                        result ~= format("            if (*cast(%s*)(func) == MFP!(MostDerivedCppClass!%s, %s.%s)) {\n",
                                cppmfp, __traits(identifier, C), __traits(identifier, C), __traits(identifier, f)); // FIXME won't work with overloads
                        result ~= format("                *result = %d;\n", signalindex++);
                        result ~= "            }\n        }\n";
                    }
                }
//                 if (!anythingUsed)
//                     result ~= "        Q_UNUSED(result);\n        Q_UNUSED(func);\n";
                result ~= "    }";
                needElse = true;
            }

//             QMultiMap<QByteArray, int> automaticPropertyMetaTypes = automaticPropertyMetaTypesHelper();
//
//             if (!automaticPropertyMetaTypes.isEmpty()) {
//                 if (needElse)
//                     result ~= " else ";
//                 else
//                     result ~= "    ";
//                 result ~= "if (_c == QMetaObject.RegisterPropertyMetaType) {\n";
//                 result ~= "        switch (_id) {\n";
//                 result ~= "        default: *cast(int*) (_a[0]) = -1; break;\n";
//                 foreach (const QByteArray &key, automaticPropertyMetaTypes.uniqueKeys()) {
//                     foreach (int propertyID, automaticPropertyMetaTypes.values(key))
//                         result ~= format("        case %d:\n", propertyID);
//                     result ~= format("            *cast(int*>(_a[0]) = qRegisterMetaType< %s ) (); break;\n", key.constData());
//                 }
//                 result ~= "        }\n";
//                 result ~= "    }\n";
//                 isUsed_a = true;
//                 needElse = true;
//             }
//
//             if (c_propertyList.length) {
//                 bool needGet = false;
//                 bool needTempVarForGet = false;
//                 bool needSet = false;
//                 bool needReset = false;
//                 for (int i = 0; i < propertyList.size(); ++i) {
//                     const PropertyDef &p = propertyList[i];
//                     needGet |= !p.read.isEmpty() || !p.member.isEmpty();
//                     if (!p.read.isEmpty() || !p.member.isEmpty())
//                         needTempVarForGet |= (p.gspec != PropertyDef.PointerSpec
//                                               && p.gspec != PropertyDef.ReferenceSpec);
//
//                     needSet |= !p.write.isEmpty() || (!p.member.isEmpty() && !p.constant);
//                     needReset |= !p.reset.isEmpty();
//                 }
//                 result ~= "\n#ifndef QT_NO_PROPERTIES\n    ";
//
//                 if (needElse)
//                     result ~= "else ";
//                 result ~= "if (_c == QMetaObject.ReadProperty) {\n";
//                 if (needGet) {
//                     if (hasQObject) {
//                         static if (QT_NO_DEBUG)
//                             result ~= "        assert(staticMetaObject.cast(_o));\n";
//                         result ~= format("        %s *_t = cast(%s *) (_o);\n", __traits(identifier, C), __traits(identifier, C));
//                     } else {
//                         result ~= format("        %s *_t = cast(%s *) (_o);\n", __traits(identifier, C), __traits(identifier, C));
//                     }
//                     if (needTempVarForGet)
//                         result ~= "        void *_v = _a[0];\n";
//                     result ~= "        switch (_id) {\n";
//                     for (int propindex = 0; propindex < propertyList.size(); ++propindex) {
//                         const PropertyDef &p = propertyList[propindex];
//                         if (p.read.isEmpty() && p.member.isEmpty())
//                             continue;
//                         QByteArray prefix = "_t.";
//                         if (p.inPrivateClass.size()) {
//                             prefix += p.inPrivateClass + ".";
//                         }
//                         if (p.gspec == PropertyDef.PointerSpec)
//                             result ~= format("        case %d: _a[0] = const_cast<void*>(cast(const void*) (%s%s())); break;\n",
//                                     propindex, prefix.constData(), p.read.constData());
//                         else if (p.gspec == PropertyDef.ReferenceSpec)
//                             result ~= format("        case %d: _a[0] = const_cast<void*>(cast(const void*) (&%s%s())); break;\n",
//                                     propindex, prefix.constData(), p.read.constData());
//                         else if (enumDeclarations.value(p.type, false))
//                             result ~= format("        case %d: *cast(int*) (_v) = QFlag(%s%s()); break;\n",
//                                     propindex, prefix.constData(), p.read.constData());
//                         else if (!p.read.isEmpty())
//                             result ~= format("        case %d: *cast( %s*) (_v) = %s%s(); break;\n",
//                                     propindex, p.type.constData(), prefix.constData(), p.read.constData());
//                         else
//                             result ~= format("        case %d: *cast( %s*) (_v) = %s%s; break;\n",
//                                     propindex, p.type.constData(), prefix.constData(), p.member.constData());
//                     }
//                     result ~= "        default: break;\n";
//                     result ~= "        }\n";
//                 }
//
//                 result ~= "    }";
//
//                 result ~= " else ";
//                 result ~= "if (_c == QMetaObject.WriteProperty) {\n";
//
//                 if (needSet) {
//                     if (hasQObject) {
//                         static if (QT_NO_DEBUG)
//                             result ~= "        assert(staticMetaObject.cast(_o));\n";
//                         result ~= format("        %s *_t = cast(%s *) (_o);\n", __traits(identifier, C), __traits(identifier, C));
//                     } else {
//                         result ~= format("        %s *_t = cast(%s *) (_o);\n", __traits(identifier, C), __traits(identifier, C));
//                     }
//                     result ~= "        void *_v = _a[0];\n";
//                     result ~= "        switch (_id) {\n";
//                     for (int propindex = 0; propindex < propertyList.size(); ++propindex) {
//                         const PropertyDef &p = propertyList[propindex];
//                         if (p.constant)
//                             continue;
//                         if (p.write.isEmpty() && p.member.isEmpty())
//                             continue;
//                         QByteArray prefix = "_t.";
//                         if (p.inPrivateClass.size()) {
//                             prefix += p.inPrivateClass + ".";
//                         }
//                         if (enumDeclarations.value(p.type, false)) {
//                             result ~= format("        case %d: %s%s(QFlag(*cast(int*) (_v))); break;\n",
//                                     propindex, prefix.constData(), p.write.constData());
//                         } else if (!p.write.isEmpty()) {
//                             result ~= format("        case %d: %s%s(*cast( %s*) (_v)); break;\n",
//                                     propindex, prefix.constData(), p.write.constData(), p.type.constData());
//                         } else {
//                             result ~= format("        case %d:\n", propindex);
//                             result ~= format("            if (%s%s != *cast( %s*) (_v)) {\n",
//                                     prefix.constData(), p.member.constData(), p.type.constData());
//                             result ~= format("                %s%s = *cast( %s*) (_v);\n",
//                                     prefix.constData(), p.member.constData(), p.type.constData());
//                             if (!p.notify.isEmpty() && p.notifyId != -1) {
//                                 const FunctionDef &f = signalList.at(p.notifyId);
//                                 if (f.arguments.size() == 0)
//                                     result ~= format("                Q_EMIT _t.%s();\n", p.notify.constData());
//                                 else if (f.arguments.size() == 1 && f.arguments[0].normalizedType == p.type)
//                                     result ~= format("                Q_EMIT _t.%s(%s%s);\n",
//                                             p.notify.constData(), prefix.constData(), p.member.constData());
//                             }
//                             result ~= "            }\n";
//                             result ~= "            break;\n";
//                         }
//                     }
//                     result ~= "        default: break;\n";
//                     result ~= "        }\n";
//                 }
//
//                 result ~= "    }";
//
//                 result ~= " else ";
//                 result ~= "if (_c == QMetaObject.ResetProperty) {\n";
//                 if (needReset) {
//                     if (hasQObject) {
//                         static if (QT_NO_DEBUG)
//                             result ~= "        assert(staticMetaObject.cast(_o));\n";
//                         result ~= format("        %s *_t = cast(%s *) (_o);\n", __traits(identifier, C), __traits(identifier, C));
//                     } else {
//                         result ~= format("        %s *_t = cast(%s *) (_o);\n", __traits(identifier, C), __traits(identifier, C));
//                     }
//                     result ~= "        switch (_id) {\n";
//                     for (int propindex = 0; propindex < propertyList.size(); ++propindex) {
//                         const PropertyDef &p = propertyList[propindex];
//                         if (!p.reset.endsWith(')'))
//                             continue;
//                         QByteArray prefix = "_t.";
//                         if (p.inPrivateClass.size()) {
//                             prefix += p.inPrivateClass + ".";
//                         }
//                         result ~= format("        case %d: %s%s; break;\n",
//                                 propindex, prefix.constData(), p.reset.constData());
//                     }
//                     result ~= "        default: break;\n";
//                     result ~= "        }\n";
//                 }
//                 result ~= "    }";
//                 result ~= "\n#endif // QT_NO_PROPERTIES";
//                 needElse = true;
//             }

            if (needElse)
                result ~= "\n";

            return result;
        }

//         static string getStaticMetaObject()
//         {
//         //
//         // Finally create and initialize the static meta object
//         //
//             fprintf(out, "const QMetaObject %s::staticMetaObject = {\n", cdef->qualified.constData());
//
//             if (cdef->superclassList.size() && (!cdef->hasQGadget || knownGadgets.contains(purestSuperClass)))
//                 fprintf(out, "    { &%s::staticMetaObject, ", purestSuperClass.constData());
//             else
//                 fprintf(out, "    { Q_NULLPTR, ");
//             fprintf(out, "qt_meta_stringdata_%s.data,\n"
//                     "      qt_meta_data_%s, ", qualifiedClassNameIdentifier.constData(),
//                     qualifiedClassNameIdentifier.constData());
//             if (hasStaticMetaCall)
//                 fprintf(out, " qt_static_metacall, ");
//             else
//                 fprintf(out, " Q_NULLPTR, ");
//
//             if (extraList.isEmpty())
//                 fprintf(out, "Q_NULLPTR, ");
//             else
//                 fprintf(out, "qt_meta_extradata_%s, ", qualifiedClassNameIdentifier.constData());
//             fprintf(out, "Q_NULLPTR}\n};\n\n");
//         }

        //
        // Generate smart cast function
        //
        static string genMetaCastBody()
        {
            string result;

            result ~= "    import core.stdc.string : strcmp;\n\n";

            result ~= "    if (!_clname) return null;\n";
            result ~= "    if (!strcmp(_clname, &qt_meta_stringdata.stringdata0[0]))\n" ~
                        "        return cast(void*) this;\n";

        /*
        //     for (int i = 1; i < cdef.superclassList.length; ++i) { // for all superclasses but the first one
        //         if (cdef.superclassList[i].second == FunctionDef.Private)
        //             continue;
        //         const char *cname = cdef.superclassList[i].first.constData();
        //         result ~= "    if (!strcmp(_clname, \"%s\"))\n        return cast( %s*>(const_cast< %s*) (this));\n",
        //                 cname, cname, __traits(identifier, C));
        //     }
        //     for (int i = 0; i < cdef.interfaceList.length; ++i) {
        //         const QList<ClassDef.Interface> &iface = cdef.interfaceList[i];
        //         for (int j = 0; j < iface.length; ++j) {
        //             result ~= "    if (!strcmp(_clname, %s))\n        return ", iface[j].interfaceId.constData());
        //             for (int k = j; k >= 0; --k)
        //                 result ~= "cast( %s*) (", iface[k].className.constData());
        //             result ~= "const_cast< %s*>(this)%s;\n",
        //                     __traits(identifier, C), QByteArray(j+1, ')').constData());
        //         }
        //     }
        */
//             if (!purestSuperClass.isEmpty()) {
//                 QByteArray superClass = purestSuperClass;
                result ~= "    return super.qt_metacast(_clname);\n";
//             } else {
//                 result ~= "    return null;\n";
//             }
            return result;
        }

        //
        // Generate internal qt_metacall()  function
        //
        static string genMetaCallBody()
        {
            string result;

            static if (BaseClassesTuple!C.length)
                result ~= "    _id = super.qt_metacall(_c, _id, _a);\n";

            result ~= "    if (_id < 0)\n        return _id;\n";
            result ~= "    ";

            bool needElse = false;
            enum numInvokables = signalList.length + slotList.length + methodList.length;

            if (numInvokables) {
                needElse = true;
                result ~= "auto qthis = cast(QObject*) this;\n";
                result ~= "if (_c == QMetaObject.Call.InvokeMetaMethod) {\n";
                result ~= format("        if (_id < %d)\n", numInvokables);
                result ~= "            qt_static_metacall(qthis, _c, _id, _a);\n";
                result ~= format("        _id -= %d;\n    }", numInvokables);

                result ~= " else if (_c == QMetaObject.Call.RegisterMethodArgumentMetaType) {\n";
                result ~= format("        if (_id < %d)\n", numInvokables);

        //         if (methodsWithAutomaticTypesHelper(methodList).isEmpty())
        //             result ~= "            *cast(int*) (_a[0]) = -1;\n";
        //         else
                    result ~= "            qt_static_metacall(qthis, _c, _id, _a);\n";
                result ~= format("        _id -= %d;\n    }", numInvokables);

            }

        /*
        //     if (cdef.propertyList.length) {
        //         bool needDesignable = false;
        //         bool needScriptable = false;
        //         bool needStored = false;
        //         bool needEditable = false;
        //         bool needUser = false;
        //         for (int i = 0; i < cdef.propertyList.length; ++i) {
        //             const PropertyDef &p = cdef.propertyList[i];
        //             needDesignable |= p.designable.endsWith(')');
        //             needScriptable |= p.scriptable.endsWith(')');
        //             needStored |= p.stored.endsWith(')');
        //             needEditable |= p.editable.endsWith(')');
        //             needUser |= p.user.endsWith(')');
        //         }
        //
        //         result ~= "\n#ifndef QT_NO_PROPERTIES\n   ");
        //         if (needElse)
        //             result ~= "else ");
        //         result ~=
        //             "if (_c == QMetaObject.Call.ReadProperty || _c == QMetaObject.Call.WriteProperty\n"
        //             "            || _c == QMetaObject.Call.ResetProperty || _c == QMetaObject.Call.RegisterPropertyMetaType) {\n"
        //             "        qt_static_metacall(this, _c, _id, _a);\n"
        //             "        _id -= %d;\n    }", cdef.propertyList.length);
        //
        //         result ~= " else ");
        //         result ~= "if (_c == QMetaObject.Call.QueryPropertyDesignable) {\n";
        //         if (needDesignable) {
        //             result ~= "        bool *_b = cast(bool*) _a[0];\n";
        //             result ~= "        switch (_id) {\n";
        //             for (int propindex = 0; propindex < cdef.propertyList.length; ++propindex) {
        //                 const PropertyDef &p = cdef.propertyList[propindex];
        //                 if (!p.designable.endsWith(')'))
        //                     continue;
        //                 result ~= "        case %d: *_b = %s; break;\n",
        //                          propindex, p.designable.constData());
        //             }
        //             result ~= "        default: break;\n";
        //             result ~= "        }\n";
        //         }
        //         result ~=
        //                 "        _id -= %d;\n"
        //                 "    }", cdef.propertyList.length);
        //
        //         result ~= " else ");
        //         result ~= "if (_c == QMetaObject.Call.QueryPropertyScriptable) {\n";
        //         if (needScriptable) {
        //             result ~= "        bool *_b = cast(bool*) _a[0];\n";
        //             result ~= "        switch (_id) {\n";
        //             for (int propindex = 0; propindex < cdef.propertyList.length; ++propindex) {
        //                 const PropertyDef &p = cdef.propertyList[propindex];
        //                 if (!p.scriptable.endsWith(')'))
        //                     continue;
        //                 result ~= "        case %d: *_b = %s; break;\n",
        //                          propindex, p.scriptable.constData());
        //             }
        //             result ~= "        default: break;\n";
        //             result ~= "        }\n";
        //         }
        //         result ~=
        //                 "        _id -= %d;\n"
        //                 "    }", cdef.propertyList.length);
        //
        //         result ~= " else ");
        //         result ~= "if (_c == QMetaObject.Call.QueryPropertyStored) {\n";
        //         if (needStored) {
        //             result ~= "        bool *_b = cast(bool*) _a[0];\n";
        //             result ~= "        switch (_id) {\n";
        //             for (int propindex = 0; propindex < cdef.propertyList.length; ++propindex) {
        //                 const PropertyDef &p = cdef.propertyList[propindex];
        //                 if (!p.stored.endsWith(')'))
        //                     continue;
        //                 result ~= "        case %d: *_b = %s; break;\n",
        //                          propindex, p.stored.constData());
        //             }
        //             result ~= "        default: break;\n";
        //             result ~= "        }\n";
        //         }
        //         result ~=
        //                 "        _id -= %d;\n"
        //                 "    }", cdef.propertyList.length);
        //
        //         result ~= " else ");
        //         result ~= "if (_c == QMetaObject.QueryPropertyEditable) {\n";
        //         if (needEditable) {
        //             result ~= "        bool *_b = cast(bool*) _a[0];\n";
        //             result ~= "        switch (_id) {\n";
        //             for (int propindex = 0; propindex < cdef.propertyList.length; ++propindex) {
        //                 const PropertyDef &p = cdef.propertyList[propindex];
        //                 if (!p.editable.endsWith(')'))
        //                     continue;
        //                 result ~= "        case %d: *_b = %s; break;\n",
        //                          propindex, p.editable.constData());
        //             }
        //             result ~= "        default: break;\n";
        //             result ~= "        }\n";
        //         }
        //         result ~=
        //                 "        _id -= %d;\n"
        //                 "    }", cdef.propertyList.length);
        //
        //
        //         result ~= " else ");
        //         result ~= "if (_c == QMetaObject.Call.QueryPropertyUser) {\n";
        //         if (needUser) {
        //             result ~= "        bool *_b = cast(bool*) _a[0];\n";
        //             result ~= "        switch (_id) {\n";
        //             for (int propindex = 0; propindex < cdef.propertyList.length; ++propindex) {
        //                 const PropertyDef &p = cdef.propertyList[propindex];
        //                 if (!p.user.endsWith(')'))
        //                     continue;
        //                 result ~= "        case %d: *_b = %s; break;\n",
        //                          propindex, p.user.constData());
        //             }
        //             result ~= "        default: break;\n";
        //             result ~= "        }\n";
        //         }
        //         result ~=
        //                 "        _id -= %d;\n"
        //                 "    }", cdef.propertyList.length);
        //
        //         result ~= "\n#endif // QT_NO_PROPERTIES");
        //     }
        */
            if (numInvokables)
                result ~= "\n    ";
            result ~="return _id;\n";

            return result;
        }
    }

    //////////////////////////////////////

    mixin (ClassDef.genSignals());

    /+QT_TR_FUNCTIONS+/ /* translations helper */

    mixin (ClassDef.genMetaStringData());
    mixin (ClassDef.genMetaDataArray());

    static immutable QMetaObject staticMetaObject;
    shared static this() { // DMD BUG #11268 can't do this in the initializer
        staticMetaObject.d = typeof(staticMetaObject.d)(
            &super.staticMetaObject, &qt_meta_stringdata.data[0], // NOTE: this is the commonplace version, should be replaced by getStaticMetaObject() later
            qt_meta_data.ptr,  &qt_static_metacall, null, null);
    }

    extern(C++) override const const(QMetaObject)* metaObject()
    {
        auto d_ptr = QObject.d_ptr.data;
        return d_ptr.metaObject ? d_ptr.dynamicMetaObject() : &staticMetaObject;
    }

    extern(C++) override void *qt_metacast(const(char) *_clname)
    {
        mixin (ClassDef.genMetaCastBody());
    }

    extern(C++) override int qt_metacall(QMetaObject.Call _c, int _id, void **_a)
    {
        mixin (ClassDef.genMetaCallBody());
    }

public:
    /+Q_DECL_HIDDEN+/ extern(C++) static void qt_static_metacall(QObject *_o, QMetaObject.Call _c, int _id, void **_a)
    {
        mixin (ClassDef.genStaticMetaCallBody());
    }

    struct QPrivateSignal {}
}

///////////////////////////////////////////////////////////////////////////////

import (C++) QtPrivate.FunctionPointer,
        QtPrivate.HasQ_OBJECT_Macro,
        QtPrivate.CheckCompatibleArguments,
        QtPrivate.AreArgumentsCompatible,
        QtPrivate.ConnectionTypes,
        QtPrivate.QSlotObjectBase,
        QtPrivate.QSlotObject,
        QtPrivate.List_Left;

// Using QObject.connect directly probably isn't what you want as D's template argument deduction can't handle Func(T)(SomeTemplate!T arg1, T arg2)
// This wrapper also handles C++ <-> DCXX connections (which are slightly tricky since T1 might be a pointer while T2 might be class for example).
QMetaObject.Connection connect2(alias signal, alias slot, T1, T2)(T1 sender, T2 receiver,
                    ConnectionType type = ConnectionType.AutoConnection)
    if ( is(typeof(signal) == function) && is(typeof(slot) == function) )
{
    import std.traits, cpp.traits;

    static if ( is(T1 == class) || is(T1 == struct) ) {
        static assert ( is(MostDerivedCppClass!T1) );
        alias _T1 = T1;
    } else {
        alias _T1 = PointerTarget!T1;
        static assert ( __traits(isCpp, _T1) );
    }

    static if ( is(T2 == class) || is(T2 == struct) ) {
        static assert ( is(MostDerivedCppClass!T2) );
        alias _T2 = T2;
    } else {
        alias _T2 = PointerTarget!T2;
        static assert ( __traits(isCpp, _T2) );
    }

//     import std.stdio : writeln;
//     writeln("MFP!(", _T1.stringof, ", ", __traits(identifier, signal), ") = ", MFP!(_T1, signal));
//     writeln("MFP!(", _T2.stringof, ", ", __traits(identifier, slot), ") = ", MFP!(_T2, slot));

    alias Func1 = typeof(MFP!(MostDerivedCppClass!_T1, signal, true));
    alias Func2 = typeof(MFP!(MostDerivedCppClass!_T2, slot, true));

    alias SignalType = FunctionPointer!Func1;
    alias SlotType = FunctionPointer!Func2;

    static assert(HasQ_OBJECT_Macro!(SignalType.Object).Value,
                        "No Q_OBJECT in the class with the signal");

    //compilation error if the arguments does not match.
    static assert(cast(int) SignalType.ArgumentCount >= cast(int) SlotType.ArgumentCount,
                        "The slot requires more arguments than the signal provides.");
    static assert(CheckCompatibleArguments!(SignalType.Arguments, SlotType.Arguments).value,
                        "Signal and slot arguments are not compatible.");
    static assert(AreArgumentsCompatible!(SlotType.ReturnType, SignalType.ReturnType).value,
                        "Return type of the slot is not compatible with the return type of the signal.");

    const(int)* types = null;
    if (type == ConnectionType.QueuedConnection || type == ConnectionType.BlockingQueuedConnection)
        types = ConnectionTypes!(SignalType.Arguments).types();

    auto _signal = MFP!(MostDerivedCppClass!_T1, signal);
    auto _slot = MFP!(MostDerivedCppClass!_T2, slot);

    import moc.cpputils : cppNew;
    auto _slotBase = cast(QSlotObjectBase*) cppNew!(QSlotObject!(Func2, List_Left!(SignalType.Arguments, SlotType.ArgumentCount).Value,
                                        SignalType.ReturnType))(_slot); // the QSlotObject is owned by the QMetaObject.Connection object, not the GC

    return QObject.connectImpl(sender, cast(void**) &_signal,
                        receiver, cast(void**) &_slot,
                        _slotBase,
                        type, types, &_T1.staticMetaObject);

    // NOTE: QObject.connect can't be called directly because it will look for the staticMetaObject in the most
    // derived C++ class hence won't work with DCXX classes.
}
