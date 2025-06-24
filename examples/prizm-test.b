// -*- mode: simpc -*-
start() {
    extrn Bdisp_AllClr_VRAM;
    extrn Bdisp_Fill_VRAM;
    extrn Bdisp_PutDisp_DD;
    extrn Bdisp_EnableColor;
    extrn PrintPixXY;
    extrn Bdisp_Rectangle;
    auto str;
    auto color;
    auto y;

    color = 0;
    y = 0;
    str = "  Hello World from B!";

    Bdisp_EnableColor(1);

    while (1) {
        Bdisp_AllClr_VRAM();
        Bdisp_Fill_VRAM(7, 4);
        Bdisp_Rectangle(16 + y, 16 + 2 * y, 64 + y, 64 + 2 * y, 7 - color);
        PrintPixXY(1,y,str,color);
        Bdisp_PutDisp_DD();
        color = color + 1;
        y = y + 1;
        if (color > 7) {
            color = 0;
        }
        if (y > 100) y = 0;
    }
}
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
