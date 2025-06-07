/* Standard Library for the Uxn target */

fputc(c, fd) {
    extrn uxn_deo;
    uxn_deo(fd + 0x18, c); /* 0x18 - Console/write,
                              0x19 - Console/error */
}

exit(code) {
    extrn uxn_deo;
    uxn_deo(0x0f, code | 0x80); /* System/state */
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
    extrn stdout, stderr;
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
