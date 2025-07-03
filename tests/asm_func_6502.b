add8 __asm__(
    "TSX",
    "CLC",
    "ADC $0103,X",
    "RTS"
);

main() {
    /* should return 0 */
    /* TODO: replace + 0xFBB with - 0x69 
       after the other pull request is merged
    */
    return (add8(0x34, 0x35) + 0xFF97);
}
