putchar(c) {
    0xFFEF(c);
}

malloc() {
    return(0x0200);
}

// TODO: add other arguments
printf(str, x1) {
    extrn char;
    auto i, j, arg, c;
    i = 0;
    j = 0;

    arg = &x1;

    c = char(str, i);
    while (c) {
        if (c == '\n') {
            putchar(0xD); // \r
        }
        if(c == '%') {
            i += 1;
            c = char(str, i);
            if (c == 0) {
                return;
            } else if (c == 's') { /* clobbers `c`, the last one */
                while (c = char(x1, j++)) {
                    putchar(c);
                }
            }
        } else {
            putchar(c); // ECHO
        }
        i++;
        c = char(str, i);
    }
}
