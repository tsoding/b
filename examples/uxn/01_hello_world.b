putstr(string) {
    extrn char, uxn_deo;
    auto i, c;
    i = 0;
    c = char(string, i);
    while (c != 0) {
        uxn_deo(0x18, c); /* 0x18 - Console/write */
        i += 1;
        c = char(string, i);
    }
}

main() {
    extrn lchar;
    auto string;
    string = "Hello world\n";
    putstr(string);
    lchar(string, 6, 'B'); /* mutable data section */
    putstr(string);
}
