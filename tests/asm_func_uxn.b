add __asm__(
    "lit 0x04",
    "ldz2",
    "lit 0x06",
    "ldz2",
    "add2",
    "lit 0x04",
    "stz2",
    "jmp2r"
);

main() {
    extrn printf;
    printf("%d\n", add(34, 35));
}
