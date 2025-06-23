add __asm__(
    "addq %rsi, %rdi",
    "movq %rdi, %rax",
    "ret"
);

main() {
    extrn printf;
    printf("%d\n", add(34, 35));
}
