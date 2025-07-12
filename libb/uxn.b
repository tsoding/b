/* Standard Library for the Uxn target */

fputc(c, fd) {
    extrn uxn_deo;
    uxn_deo(fd + 0x18, c); /* 0x18 - Console/write,
                              0x19 - Console/error */
}

putchar(c) {
    fputc(c, 0);
}

exit(code) {
    extrn uxn_deo;
    uxn_deo(0x0f, code | 0x80); /* System/state */
}

abort() {
    extrn printf;
    printf("Aborted\n");
    exit(1);
}

_udiv(a, b) {
    extrn uxn_div2;
    return (uxn_div2(a, b));
}

// TODO: `b` has to be <32768, because of `*`
_urem(a, b) {
    return (a - _udiv(a, b) * b);
}

/* loosely based on the original code by Ken Thompson */

_fprintn(n, b, fd) {
    auto a, c;

    if(a=_udiv(n,b)) /* assignment, not test for equality */
        _fprintn(a, b, fd); /* recursive */
    c = _urem(n,b) + '0';
    if (c > '9') c += 7;
    fputc(c, fd);
}

printn(n, b) _fprintn(n, b, 0);

/* doesn't support fancy features like padding, but neither did the original in B */

/* TODO: Consider adding support for negative numbers to Uxn's printf. */
/* TODO: Consider adding support for %ul to Uxn's printf. */
fprintf(fd, string, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) {
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
            } else if (c == 'l' | c == 'z') {
                c = '%';
                goto continue;
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
        continue:;
    }
}

printf(string, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) {
    fprintf(0, string, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12);
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

/* simple bump allocator */

__alloc_ptr;

malloc(size) {
    auto ret;
    if (__alloc_ptr == 0) {
        __alloc_ptr = 0x8000; /* provide __heap_base by the compiler? */
    }
    ret = __alloc_ptr;
    __alloc_ptr += size;
    return (ret);
}

memset(addr, val, size) {
    extrn lchar;
    auto i;
    i = 0;
    while (i < size) {
        lchar(addr, i, val);
        i += 1;
    }
}

stdout; stderr;

_args_count;
_args_items;
_prog_name;

_start_with_arguments() {
    extrn uxn_dei, uxn_deo2, lchar, main;
    auto type, c;
    type = uxn_dei(0x17); /* Console/type */
    c = uxn_dei(0x12);
    if (type == 2) { /* argument */
        lchar(__alloc_ptr++, 0, c);
    } else if (type == 3) { /* argument spacer */
        lchar(__alloc_ptr++, 0, 0);
        *(_args_items + (_args_count++)*2) = __alloc_ptr;
    } else if (type == 4) { /* arguments end */
        lchar(__alloc_ptr++, 0, 0);
        uxn_deo2(0x10, 0);
        exit(main(_args_count, _args_items));
    }
}

_start() {
    extrn main, uxn_dei, uxn_deo2;
    __alloc_ptr = 0x8000;
    _args_items = 0x7f00; /* 128 arguments ought to be enough for everyone */
    stdout = 0;
    stderr = 1;
    _prog_name = "-"; /* we don't have access to it */
    *_args_items = _prog_name;
    _args_count = 1;
    if (uxn_dei(0x17) != 0) {
        *(_args_items + (_args_count++)*2) = __alloc_ptr;
        uxn_deo2(0x10, &_start_with_arguments);
    } else {
        exit(main(_args_count, _args_items));
    }
}

strlen(s) {
    auto n;
    n = 0;
    while (*s++) n++;
    return (n);
}

toupper(c) {
    if ('a' <= c & c <= 'z') return (c - 'a' + 'A');
    return (c);
}
