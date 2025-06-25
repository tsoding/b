start() {
    // This is the part where we're left to manage most pre-init things
    // like enabling 16-bit colors (the OS makes the display run in a limited 8-color mode before anything ever 
    // happens).
    extrn Bdisp_AllClr_VRAM, Bdisp_EnableColor;
    extrn memcpy, memset;

    extrn data_phystart, data_vrtstart, data_size;
    extrn bss_vrtstart, bss_size;

    // Load in the data section
    memcpy(&data_vrtstart, &data_phystart, &data_size);
    memset(&bss_vrtstart, 0, &bss_size);

    Bdisp_EnableColor(1);
    Bdisp_AllClr_VRAM();

    return (main());
}


// getmchar(addr)
getmchar __asm__( "mov.b @r4, r0", "rts", "nop" );
// setmchar(addr, val)
setmchar __asm__( "mov.b r5, @r4", "rts", "nop" );

memset(vrt, byte, size) {
    while (size >= 0) {
        setmchar(vrt, byte);
        vrt = vrt + 1;
        size = size - 1;
    }
}
memcpy(vrt, phy, size) {
    auto byte;
    while (size >= 0) {
        byte = getmchar(phy);
        setmchar(vrt, byte);
        phy = phy + 1;
        vrt = vrt + 1;
        size = size - 1;
    }
}
toupper(c) {
    if (c < 'a') {
        return (c);
    }
    if (c > 'z') {
        return (c);
    }
    return (c - 'a' + 'A');
}

// Syscall-specific functions
Bdisp_EnableColor __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x0921"
);
Bdisp_AllClr_VRAM __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x025F"
);
Bdisp_PutDisp_DD __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x025F"
);

PrintPixXY __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x18F7"
);
Bdisp_Rectangle __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x0924"
);
Bdisp_Fill_VRAM __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x0275"
);
