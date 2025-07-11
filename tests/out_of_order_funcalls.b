// Introduce by https://github.com/tsoding/b/pull/198
main() {
    foo();
}

foo() {
    extrn printf;
    printf("No forward declaration is required\n");
}
