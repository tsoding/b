// -*- mode: simpc -*-

// TODO: is it possible to make seq and echo examples work on Uxn target?
//   Maybe there is some sort of mechanism that enables passing command line arguments?
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
