exit(code) {
    (*0xFFFC)(code);
}
abort() {
    exit(69);
}
assert(cond, msg) {
    if (cond == 0) {
        printf("Assertion failed: %s\n", msg);
        abort();
    }
}

/* not correct at all */
usleep(n) {
    auto i, j;
    if (n < 0) n = 32767;
    i = 0; while (i < n>>2) {
        __asm__("nop");
        i++;
    }
}

/* IO functions */
putchar(c) {
    extrn lchar;
    lchar(0xD012, 0, c);
}

getchar() {
    extrn char;
    return (char(0xD010, 0));
}

read(fd, buf, n) {
    extrn lchar;
    auto i, c;

    assert(fd == 0, "only fd==0 supported for read");
    i = 0; while (i < n) {
        c = getchar();
        lchar(buf, i, c);
        if (c == '\n') {
            return (n); /* simulate terminal behaviour */
        }
        if (c == 0xFF) return (i);
        i++;
    }

    return (n);
}

char __asm__(
    "TSX",
    "CLC",
    "ADC $0103,X", // i&0xFF
    "STA $00", // we can safely use zero-page, as our assembler
               // doesn't expect it to be preserved across op-boundaries
    "TYA",
    "ADC $0104,X", // i&0xFF00 >> 8
    "STA $01",
    "LDY #0",
    "LDA ($00),Y",
    "RTS"
);

lchar __asm__(
    "TSX",
    "CLC",
    "ADC $0103,X", // i&0xFF
    "STA $00", // we can safely use zero-page, as our assembler
               // doesn't expect it to be preserved across op-boundaries
    "TYA",
    "ADC $0104,X", // i&0xFF00 >> 8
    "STA $01",
    "LDA $0105,X",
    "LDY #0",
    "STA ($00),Y",
    "RTS"
);

/* TODO: fd not supported */
fputc(c, fd) {
    putchar(c);
}
fflush(); /* nop */

printn(n, b, sign) {
    auto a, c, d, __div, __rem;

    /* use correct div/rem based on sign */
    __div = sign ? &_div : &_udiv;
    __rem = sign ? &_rem : &_urem;

    if (sign & n < 0) {
        putchar('-');
        n = -n;
    }

    if(a=__div(n, b)) /* assignment, not test for equality */
        printn(a, b, 0); /* recursive */
    c = __rem(n,b) + '0';
    if (c > '9') c += 7;
    putchar(c);
}

stdin 0;
stdout 1;
stderr 1;

fprintf(fd, str, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {
    assert((fd == 1) | (fd == 2), "only fd in [1,2] supported for fprintf");
    return (printf(str, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15));
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
            putchar(0xD); /* \r */
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
            } else if ((c == 'z') | (c == 'l')) { /* hack for %zu %lu, % */
                c = '%';
                goto continue;
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
        continue:;
    }
}

/* Math functions */
_rand_seed 123456789;
rand() {
    _rand_seed = 20077 * _rand_seed + 12345;
    return (_rand_seed);
}

atoi(str) {
    extrn char;

    auto i, c, n, neg;
    neg = 0;
    n = 0;

    if (char(str, 0) == '-') {
        neg = 1;
        str++;
    }

    i = 0; while (1) {
        c = char(str, i);
        if ((c == 0) | (c < '0') | (c > '9')) goto end;
        n = n * 10 + (c - '0');
        i++;
    }

    end:;
    if (neg) n = -n;

    return (n);
}

/* TODO: Try to implement this function with assembly
   Problem with this implementation is that it is not
   mapped to the operator
   We cannot call this function `div` as it conflicts
   with the `divmod` test
*/
_div(a, b) {
    auto d, sign;
    sign = 0;
    if (a < 0) {
        sign = !sign;
        a = -a;
    }
    if (b < 0) {
        sign = !sign;
        b = -a;
    }

    d = 0; while(a >= b) {
        a = a - b;
        d++;
    }
    if (sign) d = -d;
    return (d);
}
_udiv(a, b) {
    auto d;
    d = 0; while(a >= b | a < 0) {
        a = a - b;
        d++;
    }
    return (d);
}

/* TODO: Try to implement this function with assembly
   Problem with this implementation is that it is not
   mapped to the operator */
_rem (a, b) {
    return (a - a / b * b);
}
_urem(a, b) {
    auto d;
    while(a >= b | a < 0) {
        a = a - b;
    }
    return (a);
}

/* memory related functions */
strlen(s) {
    auto n;
    n = 0;
    while (char(s, n)) n++;
    return (n);
}

toupper(c) {
    if ('a' <= c & c <= 'z') return (c - 'a' + 'A');
    return (c);
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

memcpy(dest, src, n) {
    extrn char, lchar;
    auto i, rn;
    rn = n;
    if (n & 1) n--;

    i = 0; while (i < n) {
        dest[i] = src[i];
        i++;
    }

    if (rn & 1) lchar(dest, n-1, char(src, n-1));
    return (dest);
}

/* TODO: actually allocate something */
__heap_ptr 0x0E000;
malloc(size) {
    extrn printf;
    auto ptr;
    ptr = __heap_ptr;
    __heap_ptr += size;
    if (__heap_ptr >= 0xFF00) {
        printf("Allocation reached end: %p\nTODO: allow allocating more, implement free\n", __heap_ptr);
        abort();
    }
    return (ptr);
}
free() {
    /* TODO: free someting */
}
realloc(ptr, size) {
    auto nptr;
    nptr = malloc(size);
    memcpy(nptr, ptr, size);
    free(ptr);
    return (nptr);
}
