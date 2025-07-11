hello() {
    putchar(72);
    putchar(69);
    auto a;
    auto b;
    a = 76; putchar(a);
    b = a;  putchar(b);
    putchar(79);
    putchar(79);
    putchar('O');
    putchar('O');
    putchar('O');
    putchar(10);
}

main() {
    hello();
    hello();
    hello();
}
