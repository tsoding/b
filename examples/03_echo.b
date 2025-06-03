main(argc, argv) {
    extrn printf;

    auto W;
    W = 8;  // TODO: breaks Uxn target

    auto first;
    first = 1;
    argv += W;
    argc -= 1;
    while (argc) {
        if (!first) printf(" ");
        printf("%s", *argv);
        first = 0;
        argv += W;
        argc -= 1;
    }
    printf("\n");
}