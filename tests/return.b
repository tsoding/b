nop() {
    return;
}

add(a, b) {
    return (a + b);
    return;
}

try_ret() {
    goto skip;
    return (-1);
skip:
    return (69);
}

main() {
    extrn printf;
    nop();
    printf("%d\n", try_ret());
    printf("%d\n", add(34, 35));
    return (0);
}
