main() {
    extrn printf;
    auto a, b, i;
    a = 10;
    i = 1;
    b = 1;
    while (i < a + 1) {
        b = b * i;
        i = i + 1;
    }
    printf("factorial(%d) = %d\n", a, b);
}
