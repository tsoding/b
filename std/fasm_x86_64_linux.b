/* Standard Library for the fasm_x86_64_linux target */

char(string, i) {
    __asm__("mov al, [rdi + rsi]");
}

lchar(string, i, c) {
    __asm__("mov [rdi + rsi], dl");
}

read(fd, buffer, count) {
    __asm__("mov rax, 0", "syscall");
}

write(fd, buffer, count) {
    __asm__("mov rax, 1", "syscall");
}

exit(code) {
    __asm__("mov rax, 60", "syscall");
}

_start() {
    // argc and argv from the caller's stack
    auto argc, argv;
    __asm__(
        "mov rax, [rbp+8]",  // argc
        "mov [rbp-8], rax", 
        "lea rax, [rbp+16]", // argv
        "mov [rbp-16], rax"
    );
    exit(main(argc, argv));
}

// TODO: Could move to libb.b if coordinated with uxn.b.
fputc(c, fd) {
    write(fd, &c, 1);
}
