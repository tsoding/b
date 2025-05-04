main() {
    extrn putchar;
    auto a;
    a = 0;
    while (a < 10) {
        putchar(a + 48);
        putchar(10);
        a = a + 1;
    }
}