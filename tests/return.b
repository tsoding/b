nop() {
    return;
}

a; // TODO: Pass as function parameter once parameters implemented.
b; // TODO: Pass as function parameter once parameters implemented.

add() {
    return (a + b);
}

main() {
    extrn printf;
    nop();
    a = 34;
    b = 35;
    printf("%d\n", add());
}
