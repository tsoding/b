variadic(a, b, c) {
    auto args, i;
    args = &c;
    extrn printf;
    i = 3; while (i > 0) printf("%d\n", args[--i]);
}

structure(args) {
    args[0] = 1;
    args[1] = 2;
    args[2] = 3;
}

main() {
    auto c, b, a;
    structure(&a);
    extrn printf;
    printf("a = %d\n", a);
    printf("b = %d\n", b);
    printf("c = %d\n", c);
    variadic(69, 420, 1337);
}
