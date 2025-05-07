// -*- mode: simpc -*-
main() {
    extrn printf;
    auto a, b, c;
    a = 0;
    b = 1;
    while (a < 1000000) {
        printf("%d\n", a);
        c = a + b;
        a = b;
        b = c;
    }
}
