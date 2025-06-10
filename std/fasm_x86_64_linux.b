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

// TODO(Tue Jun 10 12:30:15 BST 2025): This is very sensitive to the stack layout
//   above rbp. If there are any changes to the prologue inserted by the compiler,
//   this will break easily. Would be better to have some king of __attribute__((naked))
//   situation to not generate a prologue/epilogue; or to just write this in assembly directly.
// TODO: Should this be in a separate brt0? Would allow using libb and libc together,
//   at the cost of needing to also link brt0 in manually when compiling without libc.
_start() {
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
