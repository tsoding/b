main() {
    extrn printf;
    auto a;
    a = 0;
    while (a < 100) {
        printf("%02d\n", a);
        a = a + 1;
    }
}