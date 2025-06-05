W; ITEMS; COUNT; CAPACITY;

da_append(da, x) {
    extrn realloc;

    if (da[COUNT] >= da[CAPACITY]) {
        if (da[CAPACITY]) da[CAPACITY] *= 2;
        else da[CAPACITY] = 256;

        da[ITEMS] = realloc(da[ITEMS], da[CAPACITY] * W);
    }
    da[ITEMS][da[COUNT]++] = x;
}

main() {
    W = &0[1];

    auto i;
    i = 0;
    ITEMS    = i++;
    COUNT    = i++;
    CAPACITY = i++;

    // Allocating Dynamic Array structure on the stack
    auto xs_capacity, xs_count, xs_items, da;
    da = &xs_items;
    da[ITEMS]    = 0;
    da[COUNT]    = 0;
    da[CAPACITY] = 0;

    auto n; n = 10;
    i = 0; while (i < n) da_append(da, (++i)*2);

    extrn printf;
    printf("xs_items    = %p\n",  xs_items);
    printf("xs_count    = %zu\n", xs_count);
    printf("xs_capacity = %zu\n", xs_capacity);
    i = 0; while (i < n) {
        printf("%zu => %d\n", i, xs_items[i]);
        ++i;
    }
}
