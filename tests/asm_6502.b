/* This tests 6502 inline assembly with different addressing modes */

main() {
    __asm__(
        "LDA #$10",

        "LDX #10",
        "STA 10,X",   /* store at decimal 20 (10+10) */

        "LDA #$04",
        "INX",
        "STA 10,X",   /* 20:21 contains $410 */

        "LDX #69",
        "STX $420",   /* $420 contains 69 */

        "LDY #$10",
        "LDA (20),Y", /* indirect load A=((20),Y) = ((20),$10) = ($410,$10) = ($420) = 69 */

        "JMP ($FFFC)" /* jump to address in reset vector [0 in this emulator] */
    );
}
