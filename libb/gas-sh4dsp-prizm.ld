/* Linker script mostly taken from gint(public-domain) */
OUTPUT_ARCH(sh4)
OUTPUT_FORMAT(elf32-sh)
ENTRY(start)

MEMORY {
    ADDIN (rx) : ORIGIN = 0x300000, LENGTH = 2M
}
SECTIONS {
    .text : {
        *(.text.start .text .text.*)
    } > ADDIN

    /* TODO: Move this over */
    .data : {
        *(.data*)
    } > ADDIN
}
