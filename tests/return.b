nop() {
    return;
}

add(a, b) {
    return (a + b);
}

test_main() {
    extrn printf;
    nop();
    printf("%d\n", add(34, 35));
}
