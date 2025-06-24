// -*- mode: simpc -*-
main() {
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


    while (1) {
        //Bdisp_AllClr_VRAM();
        Bdisp_Fill_VRAM(7, 4);
        Bdisp_Rectangle(16 + y, 16 + 2 * y, 64 + y, 64 + 2 * y, 7 - color);
        PrintPixXY(1,y,str,color);
        Bdisp_PutDisp_DD();
        color = color + 1;
        if (color > 7) {
            color = 0;
        }

        y = y + 1;
        if (y > 110) {
            y = 0;
        }
    }
}
