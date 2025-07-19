malloc(size) {
    __asm__(
        "ldarg.0",
        "call void* [System.Runtime.InteropServices]System.Runtime.InteropServices.NativeMemory::Alloc(native uint)",
        "ret"
    );
}

memset(addr, val, size) {
    __asm__(
        "ldarg.0",
        "ldarg.2",
        "ldarg.1",
        "conv.i1",
        "call void [System.Runtime.InteropServices]System.Runtime.InteropServices.NativeMemory::Fill(void*, native uint, uint8)",
        "ldarg.0",
        "ret"
    );
}
