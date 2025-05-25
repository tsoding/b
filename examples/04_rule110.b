// -*- mode: simpc -*-

word;

display(base, n) {
    extrn printf;
    auto it, i;

    it = base;
    i  = 0;
    while (i < n) {
        if (*it) printf("#"); else printf(".");
        it += word;
        i  += 1;
    }
    printf("\n");
}

next(base, n) {
    auto it, i, state;

    it = base;
    state = *it | *(it + word) << 1;
    i  = 2;
    it = base + 2*word;
    while (i < n) {
        state <<= 1;
        state  |= *it;
        state  &= 7;
        *(it - word) = (110>>state)&1;
        i  += 1;
        it += word;
    }
}

main() {
    extrn malloc, memset;
    auto base, n;

    word = 8;
    n    = 100;
    base = malloc(word*n);
    memset(base, 0, word*n);
    *(base + (n - 2)*word) = 1;

    display(base, n);
    auto i;
    i = 0;
    while (i < n - 3) {
        next(base, n);
        display(base, n);
        i += 1;
    }
}
