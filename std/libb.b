/* Cross-Target Standard Library */

stdout 1;
stderr 2;

putchar(c) {
    extrn fputc;
    fputc(c, stdout);
}

abort() {
    extrn printf, exit;
    printf("Aborted\n");
    exit(1);
}

/* loosely based on the original code by Ken Thompson */

_fprintn(n, b, fd) {
    extrn fputc;
    auto a, c;

    if(a=n/b) /* assignment, not test for equality */
        _fprintn(a, b, fd); /* recursive */
    c = n%b + '0';
    if (c > '9') c += 7;
    fputc(c, fd);
}

printn(n, b) _fprintn(n, b, stdout);

/* doesn't support fancy features like padding, but neither did the original in B */

/* TODO: Consider adding support for negative numbers to Uxn's printf. */
/* TODO: Consider adding support for %ul to Uxn's printf. */
fprintf(fd, string, x1, x2, x3, x4) {
    extrn char, fputc;
    auto W, i, j, c, arg;
    W = &0[1];
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
                arg += W;
            }
            arg -= W;
        } else {
            fputc(c, fd);
        }
        i += 1;
        c = char(string, i);
    }
}

// TODO: More arguments once all targets support them
printf(string, x1, x2, x3, x4) {
    fprintf(stdout, string, x1, x2, x3, x4);
}

// TODO: doesn't skip whitespace, doesn't handle negative numbers
atoi(s) {
    extrn char;
    auto i, result, c;
    i = 0;
    while (1) {
        c = char(s, i++);
        if (c < '0' | c > '9') {
            goto out;
        }
        result = result * 10 + (c - '0');
    }
out:
    return (result);
}
