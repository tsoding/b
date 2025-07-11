/*
Shows how to communicate with a varvara device
*/

str "Hello world\n";

main() {
    extrn uxn_deo; /* corresponds to uxn's DEO opcode */
    extrn char;
    auto i, c;
    i = 0;
    c = char(str, i);
    while (c != 0) {
        uxn_deo(0x18, c); /* 0x18 is the Console/write device */
        c = char(str, ++i);
    }
}
