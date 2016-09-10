module moc.debugging;

import moc;

/**
* Check the output of the Q_OBJECT mixin.
*/
void debugMocClass(T)()
{
    import std.stdio : writeln;

    writeln("/////////// ", T.stringof, " ///////////\n");

    foreach (s; T.ClassDef.signalList)
        writeln("signal: ", s);
    foreach (s; T.ClassDef.slotList)
        writeln("slot: ", s);
    foreach (s; T.ClassDef.methodList)
        writeln("method: ", s);

    foreach(m; T.ClassDef.signalSeedList)
    {
        alias signalSeed = symalias!(__traits(getMember, T, m));
        writeln("\n", T.ClassDef.getSignalStr!(signalSeed._name, signalSeed._T));
    }

    writeln("\n=== genMetaStringData() ===\n", T.ClassDef.genMetaStringData(), "\n");
    writeln("\n=== genMetaDataArray() ===\n", T.ClassDef.genMetaDataArray(), "\n");
    writeln("\n=== genStaticMetaCallBody() ===\n", T.ClassDef.genStaticMetaCallBody(), "\n");
    writeln("\n=== genMetaCastBody() ===\n", T.ClassDef.genMetaCastBody(), "\n");
    writeln("\n=== genMetaCallBody() ===\n", T.ClassDef.genMetaCallBody(), "\n");
}
