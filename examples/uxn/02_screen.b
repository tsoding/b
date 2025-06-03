/* IMPORTANT! Implies no standard library. Do not link with ./std/uxn.b.
 * Because the entry point from ./std/uxn.b conflicts with the entry point
 * of this program.
 * TODO: do we want to do anything about this?
 */
logo;

x; y;
dx; dy;
size;

frameskip;

on_screen() {
    /* called 60 frames per second */
    extrn uxn_dei2, uxn_deo2, uxn_deo;
    auto width, height;
    width = uxn_dei2(0x22); /* Screen/width */
    height = uxn_dei2(0x24); /* Screen/height */
    frameskip += 1;
    if (frameskip > 2) {
        x += dx;
        y += dy;
        frameskip = 0;
    }
    if (x < 0) {
        dx = -dx;
        x = 0;
    }
    if (x + size > width) {
        dx = -dx;
        x = width - size;
    }
    if (y < 0) {
        dy = -dy;
        y = 0;
    }
    if (y + size > height) {
        dy = -dy;
        y = height - size;
    }
    uxn_deo2(0x28, 0); /* Screen/x */
    uxn_deo2(0x2a, 0); /* Screen/y */
    uxn_deo(0x2e, 0x80); /* Screen/pixel, with these params effectively "clear screen" */
    uxn_deo2(0x2c, logo); /* Screen/addr */
    uxn_deo2(0x28, x); /* Screen/x */
    uxn_deo2(0x2a, y); /* Screen/y */
    uxn_deo(0x2f, 0x81); /* Screen/sprite */
}

main() {
    extrn uxn_deo2, lchar;
    logo = "aaaaaaaaaaaaaaaa";
    lchar(logo,  0, 0x00); lchar(logo,  8, 0x7e); /*  ######  */
    lchar(logo,  1, 0x38); lchar(logo,  9, 0xc7); /* ##...### */
    lchar(logo,  2, 0x24); lchar(logo, 10, 0xdb); /* ##.##.## */
    lchar(logo,  3, 0x38); lchar(logo, 11, 0xc7); /* ##...### */
    lchar(logo,  4, 0x24); lchar(logo, 12, 0xdb); /* ##.##.## */
    lchar(logo,  5, 0x24); lchar(logo, 13, 0xdb); /* ##.##.## */
    lchar(logo,  6, 0x38); lchar(logo, 14, 0xc7); /* ##...### */
    lchar(logo,  7, 0x00); lchar(logo, 15, 0x7e); /*  ######  */
    dx = 2;
    dy = 3;
    x = 50;
    y = 72;
    size = 8;
    frameskip = 0;
    /* I have no idea how these colors work */
    uxn_deo2(0x08, 0xff0); /* System/r */
    uxn_deo2(0x0a, 0xf00); /* System/g */
    uxn_deo2(0x0c, 0xf00); /* System/b */
    uxn_deo2(0x20, &on_screen); /* Screen/vector */
}
