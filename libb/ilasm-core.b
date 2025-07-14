malloc(size) {
    __asm__(
        "ldarg.0",
        "call void* [System.Runtime.InteropServices]System.Runtime.InteropServices.NativeMemory::Alloc(native uint)",
        "ret"
    );
}
