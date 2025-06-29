/* Linker script mostly taken from gint(public-domain) 
 * NOTE: B programs will _not_ natively run on an fx-CG100 with MPM as of now!!! */
OUTPUT_ARCH(sh4)
OUTPUT_FORMAT(elf32-sh)
ENTRY(start)

MEMORY {
    /* 2MB does seem to be the limit for newer add-ins, but in general you should stick to 1MB if 
     * you ever want compatibility with older OSes/models */
    rom (rx) : o = 0x00300000, l = 2M

    /* Safe RAM that is always going to be present. */
    ram (rx) : o = 0x08100010, l = 256k
}
SECTIONS {
    . = ORIGIN(rom);
    .text : {
        *(.text.start)
        *(.text.*)
        *(.text)
    } > rom

    /* TODO: Move this over */
    . = ORIGIN(ram);
    .rodata ALIGN(4) : ALIGN(4) {
        *(.rodata*)
    } > rom
    .data ALIGN(4) : ALIGN(4) {
        data_phystart = LOADADDR(.data);
        data_vrtstart = . ;
        *(.data*)
        *(.data*)
    } > ram AT> rom
    data_size = SIZEOF(.data);
    
    .bss (NOLOAD) : {
        bss_vrtstart = . ;
    } > ram :NONE
    bss_size = SIZEOF(.bss);
}
