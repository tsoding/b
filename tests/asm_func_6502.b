add8 __asm__(
    "TSX",
    "CLC",
    "ADC $0103,X",
    "RTS"
);

gt8 __asm__(
    "LDY #$00",
    "TSX",
    "CMP $0103,X",
    "BMI minus",
    "BPL *+5", /* check relative addresses, skip LDA and RTS */
    "LDA #$69",
    "RTS",
    "plus:",
    "LDA #$01",
    "RTS",
    "minus:",
    "LDA #$00",
    "RTS"
);

main() {
    extrn printf;
    /* should return 0 */
    printf("%d\n", add8(34, 35));
    printf("%d\n", gt8(10, 1));
    printf("%d\n", gt8(1, 24));
    printf("%d\n", gt8(-1, -4));
}
