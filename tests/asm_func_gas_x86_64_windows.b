add __asm__(
    "addq %rcx, %rdx",
    "movq %rdx, %rax",
    "ret"
);

main() {
    extrn printf;
    printf("%d\n", add(34, 35));
}
