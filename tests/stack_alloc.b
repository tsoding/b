// TODO(2025-06-05 17:45:36): Ken Thompson, Users' Reference to B, 9.3
// printf implementation implies that the autovars are layed out in the memory from left to right,
// but our IR assumes right to left, due to how the stack works on the majority of modern platforms
// Consider making this more historically accurate.

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
