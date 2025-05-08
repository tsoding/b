// -*- mode: simpc -*-
main() {
    extrn InitWindow, BeginDrawing, EndDrawing, WindowShouldClose, ClearBackground;

    InitWindow(800, 600, "Hello, from B");
    while (!WindowShouldClose()) {
        BeginDrawing();
        ClearBackground(4278190335);
        EndDrawing();
    }
}
