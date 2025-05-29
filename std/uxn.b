// Standard Library for the Uxn target

// TODO: printf for uxn is not fully implemented
printf(string) {
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
