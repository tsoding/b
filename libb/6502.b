__operator__(/, _div);
__operator__(%, _rem);

exit(code) {
    0(code);
}

abort() {
    exit(69);
}

putchar(c) {
    0xFFEF(c);
}

/* TODO: fd not supported */
fputc(c, fd) {
    putchar(c);
}

/* TODO: actually allocate something */
__heap_ptr 0x0200;
malloc(size) {
    extrn printf;
    auto ptr;
    ptr = __heap_ptr;
    __heap_ptr += size;
    if (__heap_ptr >= 0x1000) {
        printf("Allocation reached end: %p\nTODO: allow allocating more, implement free\n", __heap_ptr);
        abort();
    }
    return (ptr);
}
/* TODO: free someting? */
realloc(ptr, size) {
    return (malloc(size));
}

_div(a, b) {
    auto d;
    d = 0; while(a >= b) {
        a = a - b;
        d++;
    }
    return (d);
}

/* TODO: Try to implement this function with assembly
   Problem with this implementation is that it is not
   mapped to the operator */
_rem (a, b) {
    auto d;
    while(a >= b) {
        a = a - b;
    }
    return (a);
}

printn(n, b, sign) {
    auto a, c, d;

    if (sign & n < 0) {
        putchar('-');
        n = -n;
    }

    if(a=n/b) /* assignment, not test for equality */
        printn(a, b, 0); /* recursive */
    c = n%b + '0';
    if (c > '9') c += 7;
    putchar(c);
}

printf(str, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {
    extrn char;
    auto i, j, arg, c;
    i = 0;
    j = 0;

    arg = &x1;

    c = char(str, i);
    while (c != 0) {
        if (c == '\n') {
            putchar(0xD); // \r
        }

        if(c == '%') {
            i += 1;
            c = char(str, i);
            if (c == 0) {
                return;
            } else if (c == 'd') {
                printn(*arg, 10, 1);
            } else if (c == 'u') {
                printn(*arg, 10, 0);
            } else if (c == 'p') {
                putchar('$');
                printn(*arg, 16, 0);
            } else if (c == 'c') {
                putchar(*arg);
            } else if (c == 's') { /* clobbers `c`, the last one */
                while (c = char(*arg, j++)) {
                    putchar(c);
                }
            } else if (c == 'z') { /* hack for %zu */
                c = '%';
                goto while_end;
            } else {
                putchar('%');
                arg += 2; /* word size */
            }
            arg -= 2; /* word size */
        } else {
            putchar(c); /* ECHO */
        }
        i++;
        c = char(str, i);
        while_end:
    }
}

strlen(s) {
    extrn char;
    auto n;
    n = 0;
    while (char(s, n)) n++;
    return (n);
}

toupper(c) {
    if ('a' <= c & c <= 'z') return (c - 'a' + 'A');
    return (c);
}


/* memory related functions */
memset(addr, val, size) {
    extrn lchar;
    auto i;
    i = 0;
    while (i < size) {
        lchar(addr, i, val);
        i += 1;
    }
}
