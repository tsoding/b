da_append(items, count, capacity, x) {
    extrn realloc;

    if ((*count) >= (*capacity)) {
        if ((*capacity)) (*capacity) *= 2;
        else (*capacity) = 256;

        (*items) = realloc((*items), (*capacity));
    }
    (*items)[(*count)++] = x;
}

main() {
    auto xs_items, xs_count, xs_capacity, i;
    xs_items = 0;
    xs_count = 0;
    xs_capacity = 0;
    i = 0; while (i < 10) da_append(&xs_items, &xs_count, &xs_capacity, (++i)*2);

    extrn printf;
    printf("xs_items    = %p\n",  xs_items);
    printf("xs_count    = %zu\n", xs_count);
    printf("xs_capacity = %zu\n", xs_capacity);
    i = 0; while (i < 10) {
        printf("%zu => %d\n", i, xs_items[i]);
        ++i;
    }
}
