main() {
    extrn foo, bar;
    foo();
    bar();
}

foo() {
    extrn printf;
    printf("Foo\n");
}

bar() {
    extrn printf;
    printf("Bar\n");
}