malloc(size) {
    __asm__(
        "ldarg.0",
        "call native int [mscorlib]System.Runtime.InteropServices.Marshal::AllocHGlobal(native int)",
        "ret"
    );
}
