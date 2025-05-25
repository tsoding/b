// -*- mode: simpc -*-

// TODO: make examples/02_seq.b work with html-js
//   This may require implementing missing POSIX function and command line arguments.
//   We can pass command line arguments through URL query parameters.
main(argc, argv) {
    extrn printf, fprintf, stderr, atoi;

    auto W;
    W = 8;

    if (argc <= 1) {
        fprintf(stderr, "%s: missing operand\n", *argv);
        return(1);
    }

    auto i, n;
    n = atoi(*(argv + W));
    i = 1;
    while (i <= n) {
        printf("%d\n", i);
        i += 1;
    }
}
