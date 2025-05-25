nop() {
    return;
}

add(a, b) {
    return (a + b);
}

main() {
    extrn printf;
    nop();
    printf("%d\n", add(34, 35));
}
