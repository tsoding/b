putchar(x) {
    __asm__(
        "ldarg.0",
        "conv.u1",
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

usleep(us) {
    __asm__(
        "ldarg.0",
        "conv.i4",
        "ldc.i4 1000",
        "div",
        "call void [mscorlib]System.Threading.Thread::Sleep(int32)",
        "ldc.i8 0",
        "ret"
    );
}

char(s,n) {
    __asm__(
        "ldarg 0",
        "ldarg 1",
        "add",
        "ldind.i1",
        "conv.i8",
        "ret"
    );
}

strlen(s) {
    auto n;
    n = 0;
    while (char(s, n)) n++;
    return (n);
}

rand() {
    __asm__(
        "ldsfld class [mscorlib]System.Random Program::'<Random>'",
        "callvirt instance int32 [mscorlib]System.Random::Next()",
        "conv.i8",
        "ret"
    );
}

extrn printf;