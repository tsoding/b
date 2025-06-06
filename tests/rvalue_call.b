foo() {
    extrn printf;
    printf("Foo\n");
}

bar() {
    extrn printf;
    printf("Bar\n");
}

baz() {
    extrn printf;
    printf("Baz\n");
}

main() {
    extrn printf, malloc, exit;

    auto W;
    W = &0[1];

    auto funs, i, n;
    n = 4;
    funs = malloc(n * W);
    i = 0;
    funs[i++] = &foo;
    funs[i++] = &bar;
    funs[i++] = &baz;
    funs[i++] = &exit;

    i = 0; while (i < n) funs[i++](0);
}
