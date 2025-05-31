// Standard Library for the Uxn target

putchar(c) {
    extrn uxn_deo;
    uxn_deo(0x18, c); /* 0x18 - Console/write */
}

/* loosely based on the original code by Ken Thompson */

printn(n,b) {
    extrn putchar, printn;
    auto a, c;

    if(a=n/b) /* assignment, not test for equality */
        printn(a, b); /* recursive */
    c = n%b + '0';
    if (c > '9') c += 7;
    putchar(c);
}

/* doesn't support fancy features like padding, but neither did the original in B */

printf(string, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
    extrn char;
    auto i, j, c, arg;
    i = 0;
    c = char(string, i);
    arg = &x1;
    while (c != 0) {
        if (c == '%') {
            i += 1;
            c = char(string, i);
            if (c == 0) {
                return;
            } else if (c == 'x') {
                printn(*arg, 16);
            } else if (c == 'd') {
                printn(*arg, 10);
            } else if (c == 'o') {
                printn(*arg, 8);
            } else if (c == 'c') {
                putchar(*arg);
            } else if (c == 's') { /* clobbers `c`, the last one */
                while (c = char(*arg, j++)) {
                    putchar(c);
                }
            } else {
                putchar('%');
            }
            arg -= 2; /* word size */
        } else {
            putchar(c);
        }
        i += 1;
        c = char(string, i);
    }
}

/* simple bump allocator */

__alloc_ptr;

malloc(size) {
    auto ret;
    if (__alloc_ptr == 0) {
        __alloc_ptr = 0x7fff; /* provide __heap_base by the compiler? */
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
