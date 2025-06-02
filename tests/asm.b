
main() {
    extrn __asm__;
    __asm__(
    "mov rax, 69",
    "mov rsp, rbp",
    "pop rbp",
    "ret"
    );
}
