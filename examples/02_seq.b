// -*- mode: simpc -*-
main() {
    extrn printf;
    auto a;
    a = 0;
    while (a < 100) {
        printf("%d\n", a);
        a += 1;
    }
}