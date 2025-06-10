// TODO: this file is currently not included for 6502 target when `-stdlib` is enabled
//   It collides with stuff in libb.b and also 6502 target does not support
//   enough functionality to even compile libb.b. So this file is sort of stdlib
//   for 6502, but outside of the `-stdlib` flag. You have to supply it manually.

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
