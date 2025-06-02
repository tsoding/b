main() {
    extrn foo, bar, foo_msg, bar_msg;
    foo_msg = "Foo\n";
    bar_msg = "Bar\n";
    foo();
    bar();
}

foo_msg;
bar_msg;

foo() {
    extrn printf;
    printf(foo_msg);
}

bar() {
    extrn printf;
    printf(bar_msg);
}