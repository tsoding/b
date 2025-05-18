main() {
    extrn malloc, printf, memset, exit;
    auto base, it, i, j, n, word, state;

    n    = 100;
    word = 8;

    base = malloc(word*n);
    memset(base, 0, word*n);
    *(base + (n - 2)*word) = 1;

    it = base;
    i  = 0;
    while (i < n) {
        if (*it) printf("#"); else printf(".");
        it += word;
        i  += 1;
    }
    printf("\n");

    j = 0;
    while (j < n - 3) {
        it = base;
        state = *it | *(it + word) << 1;
        i  = 2;
        it = base + 2*word;
        while (i < n) {
            state <<= 1;
            state  |= *it;
            state  = state & 7;
            *(it - word) = (110>>state)&1;
            i  += 1;
            it += word;
        }

        it = base;
        i  = 0;
        while (i < n) {
            if (*it) printf("#"); else printf(".");
            it += word;
            i  += 1;
        }
        printf("\n");

        j += 1;
    }
}
