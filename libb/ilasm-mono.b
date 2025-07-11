putchar(x) {
    __asm__(
        "ldarg.0",
        "conv.u2",
        "call void class [mscorlib]System.Console::Write(char)"
    );
}
