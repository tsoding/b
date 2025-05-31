main() {
    extrn printf;
    auto i;
    i = 0;
loop:
    if (i < 5) {
        printf("%d\n", i++);
    } else {
        goto end;
    }
    goto loop;
end:
}
