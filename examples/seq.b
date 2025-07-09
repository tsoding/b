// -*- mode: simpc -*-

// TODO: doesn't work on gas-x86_64-windows target due to linking error when using stderr
main(argc, argv) {
    extrn printf, fprintf, stderr, atoi;

    if (argc <= 1) {
        fprintf(stderr, "%s: missing operand\n", argv[0]);
        return(1);
    }

    auto i, n;
    n = atoi(argv[1]);
    i = 1;
    while (i <= n) {
        printf("%d\n", i);
        i += 1;
    }
}
