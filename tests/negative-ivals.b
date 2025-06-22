main() {
    extrn printf, n, xs;
    printf("Hello, World\n");
    auto i;
    i = 0; while (i < n) printf("%d\n", xs[i++]);
}

n 3;
xs[] -1, -2, -3;
