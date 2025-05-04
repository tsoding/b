// -*- mode: simpc -*-
main() {
    extrn printf;
    extrn putchar;
    auto a, b, c;
    a = 0;
    b = 1;
    c = a < 1000000;
    putchar(c + 48);
    /*
    while (a < 1000000) {
        printf("%d\n", a);
        c = a + b;
        a = b;
        b = c;
    }
    */
}
