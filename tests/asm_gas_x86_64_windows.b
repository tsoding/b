foo() {
    __asm__(
    "movq $0, %rax",
    "movq %rbp, %rsp",
    "popq %rbp",
    "ret"
    );
}

main() {
    extrn printf;
    printf("%d\n", foo());
}
