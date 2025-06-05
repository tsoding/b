char(string, i) {
    __asm__("mov al, [rdi + rsi]");
}

write(file, buffer, count) {
    __asm__("mov rax, 1", "syscall");
}

exit(code) {
    __asm__("mov rax, 60", "syscall");
}

putchar(char) {
    write(1, &char, 1);
}

// TODO: Use the more featureful implementation from std/uxn.b. Needs passing arguments on the stack.
printf(format) {
    auto i, c; i = 0; while (c = char(format, i++)) putchar(c);
}

_start() {
    extrn main;
    exit(main());
}
