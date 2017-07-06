/**
 * D replacement of Qt's MOC (Meta-Object Compiler).
 *
 * Contributed by Elie Morisse.
 */

module moc.types;

modmap (C++) "<qglobal.h>";
modmap (C++) "<qmetatype.h>";

import
    std.conv,
    std.traits,
    std.typetuple;

public import (C++)
    QMetaType,
    QObject,
    QtCore : qreal;

// Qt expects C++ type names, so we need a map of *differing* builtin types before querying QMetaType.type()
enum string[string] DtoCXXTypeMap = [
    "byte" : "signed char",
    "ubyte" : "unsigned char",
    "wchar" : "wchar_t",
    "dchar" : "wchar_t",
    "ushort" : "unsigned short",
    "uint" : "unsigned",
    "long" : "long long",
    "ulong" : "unsigned long long",
    "real" : "long double"
];

template DtoCXXType(T)
{
    enum typename = T.stringof;
    static if (typename in DtoCXXTypeMap)
        enum DtoCXXType = DtoCXXTypeMap[typename];
    else
        enum DtoCXXType = typename;
}

// For CTFE we need to have the QMetaType::TypeName <-> [D type name] map in D code.
// Also some D builtin types have different names from their C++ equivalents.

// F is a tuple: (QMetaType::TypeName, QMetaType::TypeNameID, RealType)
string QT_FOR_EACH_STATIC_PRIMITIVE_TYPE(string function(string, string, string) F)
{
    string result;
    result ~= F("Void", "43", "void");
    result ~= F("Bool", "1", "bool");
    result ~= F("Int", "2", "int");
    result ~= F("UInt", "3", "uint");
    result ~= F("LongLong", "4", "qlonglong");
    result ~= F("ULongLong", "5", "qulonglong");
    result ~= F("Double", "6", "double");
    result ~= F("Long", "32", "long");
    result ~= F("Short", "33", "short");
    result ~= F("Char", "34", "char");
    result ~= F("ULong", "35", "ulong");
    result ~= F("UShort", "36", "ushort");
    result ~= F("UChar", "37", "uchar");
    result ~= F("Float", "38", "float");
    result ~= F("SChar", "40", "signed char");
    return result;
}

string QT_FOR_EACH_STATIC_PRIMITIVE_POINTER(string function(string, string, string) F)
{
    return F("VoidStar", "31", "void*");
}

string QT_FOR_EACH_STATIC_CORE_CLASS(string function(string, string, string) F)
{
    string result;
    result ~= F("QChar", "7", "QChar");
    result ~= F("QString", "10", "QString");
    result ~= F("QStringList", "11", "QStringList");
    result ~= F("QByteArray", "12", "QByteArray");
    result ~= F("QBitArray", "13", "QBitArray");
    result ~= F("QDate", "14", "QDate");
    result ~= F("QTime", "15", "QTime");
    result ~= F("QDateTime", "16", "QDateTime");
    result ~= F("QUrl", "17", "QUrl");
    result ~= F("QLocale", "18", "QLocale");
    result ~= F("QRect", "19", "QRect");
    result ~= F("QRectF", "20", "QRectF");
    result ~= F("QSize", "21", "QSize");
    result ~= F("QSizeF", "22", "QSizeF");
    result ~= F("QLine", "23", "QLine");
    result ~= F("QLineF", "24", "QLineF");
    result ~= F("QPoint", "25", "QPoint");
    result ~= F("QPointF", "26", "QPointF");
    result ~= F("QRegExp", "27", "QRegExp");
    result ~= F("QEasingCurve", "29", "QEasingCurve");
    result ~= F("QUuid", "30", "QUuid");
    result ~= F("QVariant", "41", "QVariant");
    result ~= F("QModelIndex", "42", "QModelIndex");
    result ~= F("QRegularExpression", "44", "QRegularExpression");
    result ~= F("QJsonValue", "45", "QJsonValue");
    result ~= F("QJsonObject", "46", "QJsonObject");
    result ~= F("QJsonArray", "47", "QJsonArray");
    result ~= F("QJsonDocument", "48", "QJsonDocument");
    result ~= F("QPersistentModelIndex", "50", "QPersistentModelIndex");
    return result;
}

string QT_FOR_EACH_STATIC_CORE_POINTER(string function(string, string, string) F)
{
    return F("QObjectStar", "39", "QObject*");
}

string QT_FOR_EACH_STATIC_CORE_TEMPLATE(string function(string, string, string) F)
{
    string result;
    result ~= F("QVariantMap", "8", "QVariantMap");
    result ~= F("QVariantList", "9", "QVariantList");
    result ~= F("QVariantHash", "28", "QVariantHash");
    result ~= F("QByteArrayList", "49", "QByteArrayList");
    return result;
}

string QT_FOR_EACH_STATIC_GUI_CLASS(string function(string, string, string) F)
{
    string result;
    result ~= F("QFont", "64", "QFont");
    result ~= F("QPixmap", "65", "QPixmap");
    result ~= F("QBrush", "66", "QBrush");
    result ~= F("QColor", "67", "QColor");
    result ~= F("QPalette", "68", "QPalette");
    result ~= F("QIcon", "69", "QIcon");
    result ~= F("QImage", "70", "QImage");
    result ~= F("QPolygon", "71", "QPolygon");
    result ~= F("QRegion", "72", "QRegion");
    result ~= F("QBitmap", "73", "QBitmap");
    result ~= F("QCursor", "74", "QCursor");
    result ~= F("QKeySequence", "75", "QKeySequence");
    result ~= F("QPen", "76", "QPen");
    result ~= F("QTextLength", "77", "QTextLength");
    result ~= F("QTextFormat", "78", "QTextFormat");
    result ~= F("QMatrix", "79", "QMatrix");
    result ~= F("QTransform", "80", "QTransform");
    result ~= F("QMatrix4x4", "81", "QMatrix4x4");
    result ~= F("QVector2D", "82", "QVector2D");
    result ~= F("QVector3D", "83", "QVector3D");
    result ~= F("QVector4D", "84", "QVector4D");
    result ~= F("QQuaternion", "85", "QQuaternion");
    result ~= F("QPolygonF", "86", "QPolygonF");
    return result;
}


string QT_FOR_EACH_STATIC_WIDGETS_CLASS(string function(string, string, string) F)
{
    return F("QSizePolicy", "121", "QSizePolicy");
}

// ### FIXME kill that set
string QT_FOR_EACH_STATIC_HACKS_TYPE(string function(string, string, string) F)
{
    return F("QMetaTypeId2!qreal.MetaType", "-1", "qreal");
}

// F is a tuple: (QMetaType::TypeName, QMetaType::TypeNameID, AliasingType, "RealType")
string QT_FOR_EACH_STATIC_ALIAS_TYPE(string function(string, string, string, string) F)
{
    string result;
    result ~= F("ULong", "-1", "ulong", "unsigned long");
    result ~= F("UInt", "-1", "uint", "unsigned int");
    result ~= F("UShort", "-1", "ushort", "unsigned short");
    result ~= F("UChar", "-1", "uchar", "unsigned char");
    result ~= F("LongLong", "-1", "qlonglong", "long long");
    result ~= F("ULongLong", "-1", "qulonglong", "unsigned long long");
    result ~= F("SChar", "-1", "signed char", "qint8");
    result ~= F("UChar", "-1", "uchar", "quint8");
    result ~= F("Short", "-1", "short", "qint16");
    result ~= F("UShort", "-1", "ushort", "quint16");
    result ~= F("Int", "-1", "int", "qint32");
    result ~= F("UInt", "-1", "uint", "quint32");
    result ~= F("LongLong", "-1", "qlonglong", "qint64");
    result ~= F("ULongLong", "-1", "qulonglong", "quint64");
    result ~= F("QVariantList", "-1", "QVariantList", "QList<QVariant>");
    result ~= F("QVariantMap", "-1", "QVariantMap", "QMap<QString,QVariant>");
    result ~= F("QVariantHash", "-1", "QVariantHash", "QHash<QString,QVariant>");
    result ~= F("QByteArrayList", "-1", "QByteArrayList", "QList<QByteArray>");
    return result;
}

string QT_FOR_EACH_STATIC_TYPE(string function(string, string, string) F)
{
    string result;
    result ~= QT_FOR_EACH_STATIC_PRIMITIVE_TYPE(F);
    result ~= QT_FOR_EACH_STATIC_PRIMITIVE_POINTER(F);
    result ~= QT_FOR_EACH_STATIC_CORE_CLASS(F);
    result ~= QT_FOR_EACH_STATIC_CORE_POINTER(F);
    result ~= QT_FOR_EACH_STATIC_CORE_TEMPLATE(F);
    result ~= QT_FOR_EACH_STATIC_GUI_CLASS(F);
    result ~= QT_FOR_EACH_STATIC_WIDGETS_CLASS(F);
    return result;
}

///////////////////////////////////////////////////////////////////////////////

string genQMetaType_Type()
{
    string result = "enum int[string] QMetaType_types = [\n";
    result ~= QT_FOR_EACH_STATIC_TYPE(
        function string(string TypeName, string Id, string Name) {
            return "    \"" ~ Name ~ "\" : " ~ Id ~ ",\n";
        });
    result ~= QT_FOR_EACH_STATIC_ALIAS_TYPE(
        function string(string TypeName, string Id, string AliasingName, string Name) {
            return "    \"" ~ Name ~ "\" : cast(int) QMetaType.Type." ~ TypeName ~ ",\n";
        });
//     result ~= QT_FOR_EACH_STATIC_HACKS_TYPE(
//         function string(string TypeName, string Id, string Name) {
//             return "    \"" ~ Name ~ "\" : " ~ TypeName ~ ",\n";
//         });
    result ~= "    \"\" : cast(int) QMetaType.Type.UnknownType\n];";
    return result;
}

mixin(genQMetaType_Type());

///////////////////////////////////////////////////////////////////////////////

template nameToQBuiltinType(string name)
{
    static if (name.length == 0)
        enum nameToQBuiltinType = 0;
    else
    {
        static if ((name in QMetaType_types) !is null)
            enum uint tp = QMetaType_types[name];
        else
            enum uint tp = cast(uint) QMetaType.Type.UnknownType;
        enum nameToQBuiltinType = tp < cast(uint) QMetaType.Type.User ? tp : cast(uint) QMetaType.Type.UnknownType;
    }
}

/*
  Returns \c true if the type is a built-in type.
*/
template isQBuiltinType(T)
{
    enum type = T.stringof;
    static if ((type in QMetaType_types) !is null)
        enum isQBuiltinType = QMetaType_types[type] < cast(int) QMetaType.Type.User;
    else
        enum isQBuiltinType = false;
}

enum metaTypeEnumValueString(int type) = to!string(cast(QMetaType.Type) type);

template registerableMetaType(T)
{
    static if ( isQBuiltinType!T )
        enum registerableMetaType = true;
    else static if ( isPointer!T )
    {
        alias Pointee = PointerTarget!T;
        static if ( is(Pointee == class) && Filter!(isQObject, BaseClassesTuple!Pointee).length )
            enum registerableMetaType = true;
        else
            enum registerableMetaType = false;
    }
    else
        enum registerableMetaType = false;
        // TODO smart pointers, one arg template, see Generator::registerableMetaType
}
private enum isQObject(T) = is(T == QObject);