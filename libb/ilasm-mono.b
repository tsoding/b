malloc(size) {
    __asm__(
        "ldarg.0",
        "call native int [mscorlib]System.Runtime.InteropServices.Marshal::AllocHGlobal(native int)",
        "ret"
    );
}

memset(addr, val, size) {
    __asm__(
        ".locals init (int32 i)",
        "ldc.i4.0",
        "stloc.0",
        "loop:",
            "ldloc.0",
            "ldarg.2",
            "bge end",
            "ldarg.0",
            "ldloc.0",
            "add",
            "ldarg.1",
            "conv.i1",
            "stind.i1",
            "ldloc.0",
            "ldc.i4.1",
            "add",
            "stloc.0",
            "br loop",
        "end:",
            "ldarg.0",
            "ret"
    );
}
