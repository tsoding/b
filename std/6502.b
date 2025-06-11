putchar(c) {
    0xFFEF(c);
}

malloc() {
    return(0x0200);
}

// TODO: add other arguments
printf(str) {
    extrn char;
    auto i, c;
    i = 0;

    c = char(str, i);
    while (c) {
        if (c == '\n') {
            putchar(0xD); // \r
        }
        putchar(c); // ECHO
        i++;
        c = char(str, i);
    }
}
