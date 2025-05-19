// -*- mode: simpc -*-

// To compile this example you need to download raylib from https://github.com/raysan5/raylib/releases
// Than pass appropriate linker flags to the b compiler:
// $ b raylib.b -L -L/path/to/raylib-version_linux_amd64/lib/ -L -l:libraylib.a -L -lm -run

main() {
    extrn InitWindow, BeginDrawing, EndDrawing, WindowShouldClose, ClearBackground, DrawRectangle, SetTargetFPS;
    auto x, y, dx, dy, sx, sy; // TODO: adding one more variable in here crashes on x86_64

    sx = sy = 100;
    x  = y  = 200;
    dx = dy = 2;
    InitWindow(800, 600, "Hello, from B");
    SetTargetFPS(60);
    while (!WindowShouldClose()) {
        x += dx;                     // B originally had =+ operator not +=. See TODO(2025-05-18 07:06:26).
        y += dy;
        BeginDrawing();
        ClearBackground(0xFF181818); // B originally does not support hex literals actually. See TODO(2025-05-18 07:06:26).
        DrawRectangle(x, y, sx, sy, 0xFF1818FF);
        EndDrawing();
    }
}
