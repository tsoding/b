main() {
    extrn InitWindow;
    extrn BeginDrawing;
    extrn EndDrawing;
    extrn ClearBackground;

    InitWindow(800, 600, "Hello, from B");
    while (1) {
        BeginDrawing();
        ClearBackground(4278190335);
        EndDrawing();
    }
}
