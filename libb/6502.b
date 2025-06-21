putchar(c) {
    0xFFEF(c);
}

/* TODO: fd not supported */
fputc(c, fd) {
    putchar(c);
}

malloc() {
    return(0x0200);
}

_fprintn(n, b, fd) {
    auto a, c;

    putchar('X');
    /* TODO: implement division */
//    if(a=n/b) /* assignment, not test for equality */
//        _fprintn(a, b, fd); /* recursive */
//    c = n%b + '0';
//    if (c > '9') c += 7;
//    fputc(c, fd);
}

/* TODO: this was copied from uxn */
/* TODO: Consider adding support for negative numbers to Uxn's printf. */
/* TODO: Consider adding support for %ul to Uxn's printf. */
fprintf(fd, string, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
    extrn char;
    auto i, j, c, arg;
    i = 0;
    j = 0;
    c = char(string, i);
    arg = &x1;
    while (c != 0) {
        if (c == '%') {
            i += 1;
            c = char(string, i);
            if (c == 0) {
                return;
            } else if (c == 'x') {
                _fprintn(*arg, 16, fd);
            } else if (c == 'd') {
                if (*arg < 0) {
                    fputc('-', fd);
                    *arg = -*arg;
                }
                _fprintn(*arg, 10, fd);
            } else if (c == 'u') {
                _fprintn(*arg, 10, fd);
            } else if (c == 'o') {
                _fprintn(*arg, 8, fd);
            } else if (c == 'c') {
                fputc(*arg, fd);
            } else if (c == 's') { /* clobbers `c`, the last one */
                while (c = char(*arg, j++)) {
                    fputc(c, fd);
                }
            } else {
                fputc('%', fd);
                arg += 2; /* word size */
            }
            arg -= 2; /* word size */
        } else {
            fputc(c, fd);
        }
        i += 1;
        c = char(string, i);
    }
}

printf(string, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
    fprintf(0, string, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

strlen(s) {
    extrn char;
    auto n;
    n = 0;
    while (char(s, n)) n++;
    return (n);
}

toupper(c) {
    if ('a' <= c && c <= 'z') return (c - 'a' + 'A');
    return (c);
}

exit(code) {
    0(code);
}
abort() {
    exit(134);
}
