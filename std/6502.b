// TODO: add other arguments
printf(str) {
    extrn char;
    auto i, c;
    i = 0;

    c = char(str, i);
    while (c) {
        if (c == '\n') {
            0xFFEF(0xD); // \r
        }
        0xFFEF(c); // ECHO
        i++;
        c = char(str, i);
    }
}
