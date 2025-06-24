start() {
    // This is the part where we're left to manage most pre-init things
    // like enabling 16-bit colors (the OS makes the display run in a limited 8-color mode before anything ever 
    // happens).
    extrn Bdisp_AllClr_VRAM;
    extrn Bdisp_EnableColor;

    Bdisp_EnableColor(1);
    Bdisp_AllClr_VRAM();

    while (1) {}

    return (main());
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
