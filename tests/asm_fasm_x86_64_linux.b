foo() {
    __asm__(
    "mov rax, 0",
    "mov rsp, rbp",
    "pop rbp",
    "ret"
    );
}

main() {
    extrn printf;
    printf("%d\n", foo());
}
