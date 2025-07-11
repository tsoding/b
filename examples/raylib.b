// -*- mode: simpc -*-

// To compile this example you need to download raylib from https://github.com/raysan5/raylib/releases
// Than pass appropriate linker flags to the b compiler.
// # Linux
//
// $ b raylib.b -L -L/path/to/raylib-version_linux_amd64/lib/ -L -l:libraylib.a -L -lm -run
//
// # Windows mingw32-w64
// > b -t gas-x86_64-windows raylib.b -L -L$HOME/opt/raylib-version_win64_mingw-w64/lib/ -L -l:libraylib.a -L -lwinmm -L -lgdi32 -run

W;

OBJ_X; OBJ_Y; OBJ_DX; OBJ_DY; OBJ_C; SIZE_OF_OBJ;
OBJS_COUNT;
OBJS;

COLORS_COUNT 6;
COLORS [6]
    // B originally does not support hex literals actually.
    0xFF1818FF,
    0xFF18FF18,
    0xFFFF1818,
    0xFFFFFF18,
    0xFFFF18FF,
    0xFF18FFFF;

update_obj(obj) {
    auto nx, ny;

    nx = obj[OBJ_X] + obj[OBJ_DX];
    if (nx < 0 | nx + 100 >= GetScreenWidth()) {
        obj[OBJ_DX] = -obj[OBJ_DX];
        obj[OBJ_C] += 1;
        obj[OBJ_C] %= COLORS_COUNT;
    } else {
        obj[OBJ_X] = nx;
    }

    ny = obj[OBJ_Y] + obj[OBJ_DY];
    if (ny < 0 | ny + 100 >= GetScreenHeight()) {
        obj[OBJ_DY] = -obj[OBJ_DY];
        obj[OBJ_C] += 1;
        obj[OBJ_C] %= COLORS_COUNT;
    } else {
        obj[OBJ_Y] = ny;
    }
}

draw_obj(obj) DrawRectangle(obj[OBJ_X], obj[OBJ_Y], 100, 100, COLORS[obj[OBJ_C]]);

main() {
    W = &0[1];

    auto i;
    i = 0;
    OBJ_X       = i++;
    OBJ_Y       = i++;
    OBJ_DX      = i++;
    OBJ_DY      = i++;
    OBJ_C       = i++;
    SIZE_OF_OBJ = i++;

    OBJS_COUNT = 10;
    OBJS = malloc(W*SIZE_OF_OBJ*OBJS_COUNT);
    i = 0; while (i < OBJS_COUNT) {
        OBJS[i*SIZE_OF_OBJ + OBJ_X]  = rand()%500;
        OBJS[i*SIZE_OF_OBJ + OBJ_Y]  = rand()%500;
        OBJS[i*SIZE_OF_OBJ + OBJ_DX] = rand()%10;
        OBJS[i*SIZE_OF_OBJ + OBJ_DY] = rand()%10;
        OBJS[i*SIZE_OF_OBJ + OBJ_C]  = rand()%COLORS_COUNT;
        ++i;
    }

    auto paused;
    paused = 0;

    InitWindow(800, 600, "Hello, from B");
    SetTargetFPS(60);
    while (!WindowShouldClose()) {
        if (IsKeyPressed(32)) {
            paused = !paused;
        }

        if (!paused) {
            i = 0; while (i < OBJS_COUNT) {
                update_obj(&OBJS[(i++)*SIZE_OF_OBJ]);
            }
        }

        BeginDrawing();
        ClearBackground(0xFF181818);
        i = 0; while (i < OBJS_COUNT) {
            draw_obj(&OBJS[(i++)*SIZE_OF_OBJ]);
        }
        EndDrawing();
    }
}

// libc
extrn malloc, rand;

// Raylib
extrn InitWindow, BeginDrawing, EndDrawing,
      WindowShouldClose, ClearBackground,
      SetTargetFPS, GetScreenWidth, GetScreenHeight,
      DrawRectangle, IsKeyPressed;
