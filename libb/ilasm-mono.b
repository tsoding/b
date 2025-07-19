malloc(size) {
    __asm__(
        "ldarg.0",
        "conv.i",
        "call native int [mscorlib]System.Runtime.InteropServices.Marshal::AllocHGlobal(native int)",
        "ret"
    );
}

memset(addr, val, size) {
    __asm__(
        ".locals init (int64 i)",
        "ldc.i8 0",
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
            "ldc.i8 1",
            "add",
            "stloc.0",
            "br loop",
        "end:",
            "ldarg.0",
            "ret"
    );
}
