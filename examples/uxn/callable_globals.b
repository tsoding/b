/*
This example doesn't need the standard library (std/uxn.b) to work.

In B there's this strange syntax to initialize a global variable,
and a few unnamed words after that global variable, to specific values.

Who knows, maybe that was intentional to put little bits of executable code like this?
*/

print_char 0x8005, /* LIT 0x05 (location of the lower byte of the first arg in our uxn ABI) */
           0x1080, /* LDZ LIT */
           0x1817, /* 0x18 (Console/write) DEO */
           0x6c00; /* JMP2r (aka return) and some bogus one byte padding, because big endian */

main() {
    print_char('B');
    print_char('\n');
}
