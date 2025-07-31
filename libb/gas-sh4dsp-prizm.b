start_r14;
txt_x;
txt_y;
idx;


// Set to 1 if this is provably a real, proper Prizm calculator,
// or 0 on a runner.
is_realcalc;

a 6;

start() {
    // This is the part where we're left to manage most pre-init things
    // like enabling 16-bit colors (the OS makes the display run in a limited 
    // 8-color mode before anything ever happens).
    extrn Bdisp_AllClr_VRAM, Bdisp_EnableColor;
    extrn Locate_OS;
    extrn memcpy, memset;

    extrn data_phystart, data_vrtstart, data_size;
    extrn bss_vrtstart, bss_size;

    extrn detect_calculator;

    // Load in the data section
    memcpy(&data_vrtstart, &data_phystart, &data_size);
    memset(&bss_vrtstart, 0, &bss_size);

    
    // HACK to get r14 through inline assembly
    __asm__ ( 
        "mov &start_r14, r8",       // Note that this doesn't compile to two bytes. At all.
        "mov.l r14, @r8"
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

    extrn exit;
    main(0,0);
    return (69);
}
L_ilram 0xE5200000;
detect_calculator __asm__ (
    "mov &L_ilram, r4",
    "mov.l @r4, r4",
    "nop",

    "mov 69, r0",
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

    "mov &start_r14, r4",
    "mov.l @r4, r4",
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
    "nop"
);

exit(code) {
    Exit(code);
}
abort() {
    exit(0xFFFF);
}
getchar() {
    if (!is_realcalc) {
        auto ilram;        
        ilram = 0xE5200000;
        return (ilram[1]);
    }
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

    if (n < 0) {
        n = -n;
        putchar('-');
    }

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
            } else if (c == 'l') {
                i += 1;
                c = char(str, i);
                if (c == 'l') {
                    i += 1;
                    c = char(str, i);
                    if (c == 'u')
                        printn(*arg, 10);
                }
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
SYS_JUMPPOINT           0x80020070;

SYS_Bdisp_PutDisp_DD   0x00000025F;
Bdisp_PutDisp_DD __asm__(
    // Prepare r2
    "mov &SYS_JUMPPOINT, r2",
    "mov.l @r2, r2", 

    "mov &SYS_Bdisp_PutDisp_DD, r0",
    "mov.l @r0, r0", 
    "jmp @r2",
    "nop",
    "rts",
    "nop"
);
SYS_PrintMiniMini   0x00000021B;
PrintMiniMini __asm__(
    // Prepare r2
    "mov &SYS_JUMPPOINT, r2",
    "mov.l @r2, r2", 

    "mov &SYS_PrintMiniMini, r0",
    "mov.l @r0, r0", 
    "jmp @r2",
    "nop",
    "rts",
    "nop"
);
SYS_Locate_OS   0x000001863;
Locate_OS __asm__(
    // Prepare r2
    "mov &SYS_JUMPPOINT, r2",
    "mov.l @r2, r2", 

    "mov &SYS_Locate_OS, r0",
    "mov.l @r0, r0", 
    "jmp @r2",
    "nop",
    "rts",
    "nop"
);
SYS_Bdisp_Fill_VRAM   0x00000275;
Bdisp_Fill_VRAM __asm__(
    // Prepare r2
    "mov &SYS_JUMPPOINT, r2",
    "mov.l @r2, r2", 

    "mov &SYS_Bdisp_Fill_VRAM, r0",
    "mov.l @r0, r0", 
    "jmp @r2",
    "nop",
    "rts",
    "nop"
);
SYS_Bdisp_AllClr_VRAM   0x00000272;
Bdisp_AllClr_VRAM __asm__(
    // Prepare r2
    "mov &SYS_JUMPPOINT, r2",
    "mov.l @r2, r2", 

    "mov &SYS_Bdisp_AllClr_VRAM, r0",
    "mov.l @r0, r0", 
    "jmp @r2",
    "nop",
    "rts",
    "nop"
);
SYS_Bdisp_EnableColor   0x00000921;
Bdisp_EnableColor __asm__(
    // Prepare r2
    "mov &SYS_JUMPPOINT, r2",
    "mov.l @r2, r2", 

    "mov &SYS_Bdisp_EnableColor, r0",
    "mov.l @r0, r0", 
    "jmp @r2",
    "nop",
    "rts",
    "nop"
);
SYS_Malloc   0x00001F44;
Malloc __asm__(
    // Prepare r2
    "mov &SYS_JUMPPOINT, r2",
    "mov.l @r2, r2", 

    "mov &SYS_Malloc, r0",
    "mov.l @r0, r0", 
    "jmp @r2",
    "nop",
    "rts",
    "nop"
);
SYS_Free   0x00001F42;
Free __asm__(
    // Prepare r2
    "mov &SYS_JUMPPOINT, r2",
    "mov.l @r2, r2", 

    "mov &SYS_Free, r0",
    "mov.l @r0, r0", 
    "jmp @r2",
    "nop",
    "rts",
    "nop"
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
    "mov      0,r3",
    "subc    r3,r2",
    "div0s   r0,r1",

    // WOE! DIVISION! (fun SuperH hjinks)
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",
    "rotcl   r2", "div1    r0, r1", "rotcl   r2", "div1    r0, r1",

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
