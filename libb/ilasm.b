putchar(x) {
    __asm__(
        "ldarg.0",
        "conv.u2",
        "call void [mscorlib]System.Console::Write(char)",
        "ldc.i8 0",
        "ret"
    );
}

getchar() {
    __asm__(
        "call int32 [mscorlib]System.Console::Read()",
        "conv.i8",
        "ret"
    );
}
