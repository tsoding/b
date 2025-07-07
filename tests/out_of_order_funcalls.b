main() {
    foo();
}

foo() {
    extrn printf;
    printf("No forward declaration is required\n");
}
