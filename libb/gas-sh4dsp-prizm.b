start_osaddress;
start_r14;
txt_x;
txt_y;
start() {
    // This is the part where we're left to manage most pre-init things
    // like enabling 16-bit colors (the OS makes the display run in a limited 8-color mode before anything ever 
    // happens).
    extrn Bdisp_AllClr_VRAM, Bdisp_EnableColor;
    extrn Locate_OS;
    extrn memcpy, memset;

    extrn data_phystart, data_vrtstart, data_size;
    extrn bss_vrtstart, bss_size;

    // HACK to get PR
    // this is probably the most cursed piece of code i have ever written, but its goal is to effectively 
    // write PR (which should be where we're _returning_) to a symbol (here start_osaddress).
    __asm__ ( 
        "sts pr, r0",
        "mov.l start_hackpraddr, r4",
        "mov.l r0, @r4",
        "mov.l start_hackskaddr, r4",
        "mov.l r14, @r4",
        "bra start_hackend",
        "nop",

        ".align 4",
        "start_hackpraddr: .long start_osaddress",
        "start_hackskaddr: .long start_r14",
        "start_hackend:"
    );

    // Load in the data section
    memcpy(&data_vrtstart, &data_phystart, &data_size);
    memset(&bss_vrtstart, 0, &bss_size);

    Bdisp_EnableColor(1);
    Bdisp_AllClr_VRAM();
    Locate_OS(1,1);
    txt_x = 1;
    txt_y = 1;

    return (main(0, 0));
}
store_pr(pr) {
    start_osaddress = pr;
}


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
exit(code) {
    extrn abort;
    abort();
}
abort() {
    // TODO: this function is broken...
    __asm__ ( 
        "mov r4, r0",
        "mov.l exit_hackskaddr, r4",
        "mov.l @r4, r15",
        "mov.l exit_hackpraddr, r4",
        "mov.l @r4, r4",
        "lds r4, pr",
        
        "rts",
        "nop",

        ".align 4",
        "exit_hackpraddr: .long start_osaddress",
        "exit_hackskaddr: .long start_r14"
    );
}
// We have no proper output "file", so we have to emulate
putchar(ch) {
    extrn Print_OS, Locate_OS, Bdisp_PutDisp_DD;
    auto str;
    str = "X";

    if (ch != '\n') {
        lchar(str, 0, ch);    
        Print_OS(str, 0, 0);
        txt_x = txt_x + 1;
    } else {
        txt_x = 1;
        txt_y = txt_y + 1;
    }
    Locate_OS(txt_x, txt_y);

    // TODO: Updating the screen like that is pretty slow
    Bdisp_PutDisp_DD();
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
malloc(size) {
    printf("No malloc?!\n");
}

// Intrisic maths (division/modulo)
intrisic_div (a, b) {
    // TODO: This could be implemented with DSP loops(do we have these enabled by default?) and div1
    auto d;
    d = 0; while(a >= b) {
        a = a - b;
        d++;
    }
    return (d);
}
intrisic_mod (a, b) {
    auto q;
    q = intrisic_div(a, b);
    return (a - (b * q));
}
