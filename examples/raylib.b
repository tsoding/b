// -*- mode: simpc -*-
main() {
    extrn InitWindow, BeginDrawing, EndDrawing, WindowShouldClose, ClearBackground, DrawRectangle, SetTargetFPS;
    auto x, y, dx, dy;

    x = 100;
    y = 100;
    dx = 1;
    dy = 1;
    InitWindow(800, 600, "Hello, from B");
    SetTargetFPS(60);
    while (!WindowShouldClose()) {
        x = x + dx;
        y = y + dy;
        BeginDrawing();
        ClearBackground(0xFF181818); // B originally does not support hex literals actually
        DrawRectangle(x, y, 100, 100, 0xFF1818FF);
        EndDrawing();
    }
}
