// This test is meant to be run with the B standard library.
// Compile with `-stdlib -L -nostdlib` to use libb and not libc.

copy(dst, src) {
    extrn char, lchar;
    auto i, c;
    i = 0;
    while (c = char(src, i)) {
        lchar(dst, i++, c);
    }
    lchar(dst, i, 0);
}

puts(s) {
    extrn char, putchar;
    auto i, c;
    i = 0;
    while (c = char(s, i++)) {
        putchar(c);
    }
}

main() {
    extrn lchar;
    auto str 15;
    copy(str, "Hello, World!\n");
    puts(str);
    lchar(str, 7, 'B');
    puts(str);
}
