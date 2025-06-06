/* Standard Library for the fasm_x86_64_linux target */

char(string, i) {
    __asm__("mov al, [rdi + rsi]");
}

lchar(string, i, c) {
    __asm__("mov [rdi + rsi], dl");
}

read(fd, buffer, count) {
    __asm__("mov rax, 0", "syscall");
}

write(fd, buffer, count) {
    __asm__("mov rax, 1", "syscall");
}

exit(code) {
    __asm__("mov rax, 60", "syscall");
}

// TODO: Need _start() with -L -nostdlib but can't have it without.
// TODO: The below are stolen from uxn.b. Should collect cross-target code into one place.

putchar(c) {
    extrn write;
    write(1, &c, 1);
}

/* loosely based on the original code by Ken Thompson */

printn(n, b) {
    extrn putchar;
    auto a, c;
    if (a = n / b) {
        printn(a, b);
    }
    c = n % b + '0';
    if (c > '9') {
        c += 7;
    }
    putchar(c);
}

/* doesn't support fancy features like padding, but neither did the original in B */

printf(string, xs) {
    extrn char, putchar, printn;
    auto W, i, j, c, arg;
    W = &0[1];
    i = 0;
    j = 0;
    c = char(string, i);
    arg = &xs;
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
            arg -= W;
        } else {
            putchar(c);
        }
        i += 1;
        c = char(string, i);
    }
}
