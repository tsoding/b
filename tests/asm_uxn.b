main() {
    extrn printf, putchar;
    auto i;
    i = 0; while (i < 3) {
        __asm__(
            "lit 'B'",
            "lit 0x18",
            "deo"
        );
        i++;
    }
    /* asm labels are global and live in a distinct namespace */
    __asm__("jmi main_skip");
    printf("Should be skipped\n");
    __asm__("main_skip:");
    putchar('\n');
}
