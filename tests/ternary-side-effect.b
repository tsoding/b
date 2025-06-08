foo() {
    extrn printf;
    printf("  Foo\n");
}

bar() {
    extrn printf;
    printf("  Bar\n");
}

test_main() {
    extrn printf;
    printf("Only Foo should be printed bellow:\n");
    1 ? foo() : bar();
}
