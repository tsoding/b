// TODO: Run with platform-specific tests

add __asm__(
    "add rdi, rsi",
    "mov rax, rdi",
    "ret"
);

main() {
    extrn printf;
    printf("%d\n", add(34, 35));
}
