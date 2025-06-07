shift(argc, argv) {
    auto W, x;
    W = &0[1];
    x = *argv[0];
    *argc -= 1;
    *argv += W;
    return (x);
}

main(argc, argv) {
    extrn printf;
    printf("argc = %d\n", argc);
    auto i; i = 0; while (argc) {
        printf("argv[%d] = %s\n", i++, shift(&argc, &argv));
    }
}
