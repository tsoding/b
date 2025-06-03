main(argc, argv) {
    extrn printf;

    auto first, i;
    first = 1;
    i = 1;
    while (i < argc) {
        if (!first) printf(" ");
        printf("%s", argv[i++]);
        first = 0;
    }
    printf("\n");
}
