main() {
    extrn malloc, printf, fgets, stdin;
    auto name, n;
    n = 256;
    name = malloc(n);
    printf("What is your name?\n");
    fgets(name, n, stdin);
    printf("Hello, %s", name);
}
