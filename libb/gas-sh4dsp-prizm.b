start_r14;
txt_x;
txt_y;
idx;


// Set to 1 if this is provably a real, proper Prizm calculator,
// or 0 on a runner.
is_realcalc;

start() {
    // This is the part where we're left to manage most pre-init things
    // like enabling 16-bit colors (the OS makes the display run in a limited 8-color mode before anything ever 
    // happens).
    extrn Bdisp_AllClr_VRAM, Bdisp_EnableColor;
    extrn Locate_OS;
    extrn memcpy, memset;

    extrn data_phystart, data_vrtstart, data_size;
    extrn bss_vrtstart, bss_size;

    extrn detect_calculator;

    // We don't call any syscalls, thus we can mostly promuise r10 will be safe

    // Load in the data section
    memcpy(&data_vrtstart, &data_phystart, &data_size);
    memset(&bss_vrtstart, 0, &bss_size);

    // HACK to get r14
    __asm__ ( 
        "mov.l start_hackskaddr, r8",
        "mov.l r14, @r8",

        "nop",
        "bra start_hackend",
        "nop",

        ".align 4",
        "start_hackskaddr: .long start_r14",
        "start_hackend:",
        "nop",
        "nop"
    );

    is_realcalc = detect_calculator();
    idx = 0;
    if (is_realcalc) {
        Bdisp_EnableColor(1);
        Bdisp_AllClr_VRAM();
        Locate_OS(1,1);
    }
    txt_x = 1;
    txt_y = 1;

    return (main(0, 0));
}
detect_calculator __asm__ (
    "mov.l .L_ilram, r4",
    "nop",
    "bra tramp_end",
    "nop",

    ".align 4",
    ".L_ilram: .long 0xE5200000",
    "tramp_end:",

    "mov #0, r0",
    "mov.l r0, @r4",
    "mov #69, r0",
    "mov.l r0, @r4",
    "mov.l @r4, r4",
    "cmp/eq r0, r4",

    "rts",
    "movt r0"
);


// getmchar(addr)
getmchar __asm__( "mov.b @r4, r0", "rts", "nop" );
// setmchar(addr, val)
setmchar __asm__( "mov.b r5, @r4", "rts", "nop" );


// Regular libb functions
char(string, idx)
{
    return (getmchar(string + idx));
}
lchar(string, i, ch) {
    setmchar(string + i, ch);
}

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
tolower(c) {
    if (c < 'A') {
        return (c);
    }
    if (c > 'Z') {
        return (c);
    }
    return (c - 'A' + 'a');
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
Exit __asm__ ( 
    "mov r4, r0",                       // CODE = return

    "mov.l exit_hackskaddr, r4",
    "mov.l @r4, r15",
    "lds.l @r15+, pr",
    "mov.l @r15+, r14",
    "mov.l @r15+, r13",
    "mov.l @r15+, r12",
    "mov.l @r15+, r11",
    "mov.l @r15+, r10",
    "mov.l @r15+, r9",
    "mov.l @r15+, r8",
    
    "rts",
    "nop",

    ".align 4",
    "exit_hackskaddr:   .long start_r14"
);
exit(code) {
    if (is_realcalc) {
        Exit(code);
    }
    __asm__ ( 
        "mov #-1, r0",
        "jmp @r0"
    );
}
abort() {
    exit(0xFFFF);
}
// We have no proper output "file", so we have to emulate
putchar(ch) {
    extrn Bdisp_Fill_VRAM, Bdisp_PutDisp_DD;
    extrn PrintMiniMini;
    auto str;
    str = "X";

    if (is_realcalc) {
        if (txt_y >= 180) {
            txt_x = 1;
            txt_y = 1;
            Bdisp_Fill_VRAM(0xFFFF, 4);
        }

        if (ch != '\n') {
            lchar(str, 0, ch);    
            PrintMiniMini(&txt_x, &txt_y, str, 0, 0, 0);

        } else {
            txt_x = 1;
            txt_y = txt_y + 10;
            // TODO: Updating the screen like that is pretty slow
            Bdisp_PutDisp_DD();
        }
    } else {
        auto ilram;        
        ilram = 0xE5200000;
        ilram[1] = ch;
    }
}

printn(n, b) {
    auto a, c, d;

    if(a=n/b) /* assignment, not test for equality */
        printn(a, b); /* recursive */
    c = (n%b) + '0';
    if (c > '9') c += 7;
    putchar(c);
}
printf(str, x1, x2, x3, s4, s5, s6, s7, s8, s9, s10, s11, s12) {
    extrn char;
    auto i, j, arg, c;
    i = 0;
    j = 0;

    arg = &x1;

    c = char(str, i);
    while (c) {
        if(c == '%') {
            i += 1;
            c = char(str, i);
            if (c == 0) {
                return;
            } else if (c == 'd') {
                printn(*arg, 10);
            } else if (c == 'p') {
                printn(*arg, 16);
            } else if (c == 'c') {
                putchar(*arg);
            } else if (c == 's') { /* clobbers `c`, the last one */
                while (c = char(*arg, j++)) {
                    putchar(c);
                }
            } else {
                putchar('%');
                arg += 4; /* word size */
            }
            arg -= 4; /* word size */
        } else {
            putchar(c); // ECHO
        }
        i++;
        c = char(str, i);
    }
}

strlen(str) {
    extrn char;
    auto i, c;
    i = 0;

    c = char(str, i);
    while (c) {
        i++;
        c = char(str, i);
    }
    return (i);
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
Locate_OS __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x1863"
);
GetVRAMAddress __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x1E6"
);
PrintMiniMini __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x021B"
);
Print_OS __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x01F9"
);

// TODO: malloc/free
Malloc __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x1F44"
);
Free __asm__(
    // Prepare r2
    "mov.l 1f, r2",
    "mov.l 2f, r0",           // syscall ID
    "jmp @r2",
    "nop",
    "rts",
    "nop",
    
    ".align 4",
    "1: .long 0x80020070",
    "2:   .long 0x1F42"
);

malloc(size) {
    auto ret;
    if (is_realcalc) {
        return (Malloc(size));
    }
    ret = 0x8140000 + idx;
    idx = idx + size * 4;
    return (ret);
}
free(ptr) {
    if (is_realcalc) {
        return (Free(ptr));
    }
}

// Intrisic maths (division/modulo)
// Division routine taken from shared-ptr.com/sh_insns.html
intrisic_div __asm__ (
    "mov r4, r2",
    "mov r5, r0",
    "mov     r2,r3",
    "rotcl   r3",
    "subc    r1,r1",
    "mov     #0,r3",
    "subc    r3,r2",
    "div0s   r0,r1",

    ".rept 32",
    "rotcl   r2",
    "div1    r0,r1",
    ".endr",

    "rotcl   r2",
    "addc    r3,r2",

    "rts",
    "mov r2, r0"
);
intrisic_mod (a, b) {
    auto q;
    q = intrisic_div(a, b);
    return (a - (b * q));
}

