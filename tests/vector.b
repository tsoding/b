test1() {
    extrn printf;
    auto xs 4, W;
    W = &0[1]; // word size

    *xs = 34;
    *(xs + 1*W) = '+';
    xs[2] = 35;
    xs[3] = 69;

    printf(
        "%d %c %d = %d\n",
        xs[0],
        xs[1],
        *(xs + 2*W),
        *(xs + 3*W)
    );
}

COUNT 3;
A "Just";
B "Testing";
C "Globals";
LIST A, B, C;
test2() {
    extrn printf;
    auto i; i = 0;
    while (i < COUNT) printf("%s\n", *((&LIST)[i++]) );
}

N  5 ;
V [5];
test3() {
    extrn printf;
    auto i;
    i = 0; while (i < N) V[i] = ++i * 2;
    i = 0; while (i < N) printf("%d => %d\n", i, V[i++]);
}


main() {
    test1();
    test2();
    test3();
}
