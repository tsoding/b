read_name(name, n) {
    extrn getchar;
    auto i;
    i = 0; while (i < n) {
        auto a;
        a = getchar();
        name[i++] = a;
        if (a == '\n') return (i);
    }
    return (n);
}

print_name(name, n) {
    extrn putchar;
    auto i;
    i = 0; while (i < n) {
        putchar(name[i++]);
    }
}

main() {
    extrn malloc, printf, getchar, putchar;
    auto name, n, i;
    n = 256;
    name = malloc(n);
    printf("What is your name?\n");
    n = read_name(name, n);
    printf("Hello, ");
    print_name(name, n);
}
