// Copyright 2025 Yui <yui300127@gmail.com>
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// tested using the B compiler made by tsoding
// https://github.com/tsoding/b
// (26/05/2025 01:49)
//
// This program uses POSIX specific functions to handle input
// also ANSI escape code for drawing to the screen
// so there's no chance to get it to run on html-js target
//
// due to the compiler still being early in development
// there are alot of "hacks" in the code to get around
// some limitations, like assuming the memory layout of
// C structures, usage of magic constants, usage of
// memset/memcopy to write specific memory regions that are
// smaller than the machine word size, replacing structs and
// array with pointers (thus having to malloc all of them), etc..
//
// also the rendering code is just stupid, it redraws the entire
// screen every single frame ...


// constants
W;
W2;
X;
Y;
STDIN;
STDOUT;
TCSAFLUSH;
FIONREAD;
TIOCGWINSZ;

// enum Tile
EMPTY;
BODY;
HEAD;
APPLE;

// enum Direction
UP;
RIGHT;
DOWN;
LEFT;

// state
screen;
screen_size;
input_ch;
bytes_remaining;
head;
facing;
body;
body_len;
score;
frame_time;
apple;
width;
height;
apple_size;
frame;
dead;
rgb;
just_died;

body_i(i) return (body + i*W2);
screen_xy(x, y) return (screen + (x + y*width)*W);

is_colliding_with_apple(pos) {
  auto x, y, dx, dy, is_colliding;
  is_colliding = 0;
  dy=0; while (dy < apple_size) {
    dx=0; while (dx < apple_size) {
      x = apple[X] + dx;
      y = apple[Y] + dy;
      is_colliding |= pos[X]==x & pos[Y]==y;
      dx += 1;
    }
    dy += 1;
  }
  return (is_colliding);
}

randomize_apple() {
  extrn rand;
  auto again; again = 1; while (again) {
    apple[X] = rand() % (width - apple_size - 2) + 1;
    apple[Y] = rand() % (height - apple_size - 2) + 1;
    again = is_colliding_with_apple(head);

    auto i; i = 0; while (i < body_len) {
      again |= is_colliding_with_apple(body_i(i));
      i += 1;
    }

  }
}

init_globals() {
  extrn malloc, memset, ioctl, srand, time;
  srand(time(0));

  W = 8;
  W2 = W*2;
  X = 0;
  Y = 1;
  STDIN = 0;
  STDOUT = 1;
  TCSAFLUSH = 2;
  TIOCGWINSZ = 21523;
  FIONREAD = 21531;

  auto ws;
  ws = malloc(16);
  ioctl(STDOUT, TIOCGWINSZ, ws);

  height = (*ws & 0xffff)-3;
  width = (*ws >> 17 & 0x7fff)-2;
  apple_size = 3;
  dead = 0;
  frame = 0;
  just_died = 0;


  EMPTY = 0;
  BODY = 1;
  HEAD = 2;
  APPLE = 4;

  UP = 0;
  RIGHT = 1;
  DOWN = 2;
  LEFT = 3;

  screen_size = width*height*W;
  screen = malloc(screen_size);
  memset(screen, EMPTY, screen_size);

  // add `&` to take address of variable
  // instead of spamming malloc everywhere
  input_ch = malloc(W);
  bytes_remaining = malloc(W);

  head = malloc(W2);
  head[X] = width>>1;
  head[Y] = height>>1;

  body = malloc(screen_size*W2);
  memset(body, 255, screen_size); // fill -1
  body_len = 5;
  score = 0;

  apple = malloc(W2);
  randomize_apple();

  frame_time = 100;
  facing = RIGHT;
}

unreachable(message) {
  extrn printf, abort;
  printf("\nUNREACHABLE: %s\n", message);
  abort();
}

render() {
  extrn printf;
  auto y, x, tile;

  // stb_c_lexer does not support hex or octal escape-
  // sequences in strings, check `stb__clex_parse_char`.

  printf("%c[H", 27);
  x=0; while (x < width+2) { x+=1;
    printf("%c[38;5;238m██", 27);
  }
  printf("%c[H", 27);
  printf("%c[48;5;238m%c[38;5;232m", 27, 27);
  printf("  Score: %-4d Body length: %-4d Frame time(ms): %d\n", score, body_len, frame_time);


  // !when for loops
  y=0; while (y < height) {
    printf("%c[38;5;238m██", 27);
    x=0; while (x < width) {
      tile = *screen_xy(x, y);
      if (tile == EMPTY) {
        printf("%c[48;5;233m  ", 27);
      } else if (tile == BODY) {
        if (just_died) printf("%c[38;5;65m██", 27);
        else if (dead) printf("%c[38;5;245m██", 27);
        else if (rgb) {
          auto r, g, b, f;
          f = frame*35;
          r = (f) % 510;
          if (r >= 256) r = 510-r;
          g = ((f>>1) + 64) % 510;
          if (g >= 256) g = 510-g;
          b = ((f>>2)*3 + 128) % 510;
          if (b >= 256) b = 510-b;
          printf("%c[38;2;%d;%d;%dm██", 27, r, g, b);
       } else printf("%c[38;5;41m██", 27);
      } else if (tile == HEAD) {
        if (just_died) printf("%c[48;5;65m", 27);
        else if (dead) printf("%c[48;5;245m", 27);
        else if (rgb) {
          auto r, g, b, f;
          f = frame*35;
          r = (f) % 510;
          if (r >= 256) r = 510-r;
          g = ((f>>1) + 64) % 510;
          if (g >= 256) g = 510-g;
          b = ((f>>2)*3 + 128) % 510;
          if (b >= 256) b = 510-b;
          printf("%c[48;2;%d;%d;%dm", 27, r, g, b);
       } else printf("%c[48;5;41m", 27);
        printf("%c[38;5;22m", 27);
        if (facing == UP) {
          printf("▀█");
        } else if (facing == DOWN) {
          printf("█▄");
        } else if (facing == RIGHT) {
          printf("▄█");
        } else if (facing == LEFT) {
          printf("█▀");
        } else {
          unreachable("in render(), autovar `facing` containt unknown Direction variant");
        }
      } else if (tile == APPLE) {
        printf("%c[38;5;196m██", 27);
      } else {
        unreachable("in render(), autovar `tile` containt unknown Tile variant");
      }
      x+=1;
    }
    printf("%c[38;5;238m██", 27);
    printf("\n");
    y+=1;
  }

  x=0; while (x < width+2) { x+=1;
    printf("%c[38;5;238m██", 27);
  }
  printf("\n");
}

orig_termios;
enable_raw_mode() {
  extrn malloc, tcgetattr, tcsetattr, printf, memset;
  orig_termios = malloc(64);
  tcgetattr(STDIN, orig_termios);

  auto raw;
  raw = malloc(64);
  memset(raw, 0, 64);
  *raw = *orig_termios & 0xfffffffffffffff5;

  tcsetattr(STDIN, TCSAFLUSH, raw);
  printf("%c[?25l", 27);
}

disable_raw_mode() {
  extrn tcsetattr, printf;
  tcsetattr(STDIN, TCSAFLUSH, orig_termios);
  printf("%c[?25h", 27);
}

handle_user_input() {
  extrn read, ioctl, usleep;
  if (just_died) {
    just_died = 0;
    usleep(1000000);
    ioctl(STDIN, FIONREAD, bytes_remaining);
    auto i; i = 0; while (i < *bytes_remaining) { i += 1;
      read(STDIN, input_ch, 1);
    }
  } else {
    auto f;
    f = facing;
    ioctl(STDIN, FIONREAD, bytes_remaining);
    while (*bytes_remaining != 0) {
      read(STDIN, input_ch, 1);
      if (*input_ch == 113) { return (1); } // q -> exit=true
      else if (!dead) {
        if (*input_ch == 119 & f != DOWN)  { facing = UP;     } // w
        if (*input_ch == 97  & f != RIGHT) { facing = LEFT;   } // a
        if (*input_ch == 115 & f != UP)    { facing = DOWN;   } // s
        if (*input_ch == 100 & f != LEFT)  { facing = RIGHT;  } // d
        if (*input_ch == 114) { rgb = !rgb; } // r
      } else {
        init_globals();
      }
      ioctl(STDIN, FIONREAD, bytes_remaining);
    }
  }
  return (0); // exit=false
}

draw_screen() {
  extrn memset;
  memset(screen, EMPTY, screen_size);

  auto i; i = 0; while (i < body_len) {
    auto segment;
    segment = body_i(i);
    if(segment[X] != -1 & segment[Y] != -1)
      *screen_xy(segment[X], segment[Y]) = BODY;
    i += 1;
  }

  *screen_xy(head[X], head[Y]) = HEAD;

  auto x, y, dx, dy;
  dy = 0; while (dy < apple_size) {
    dx = 0; while (dx < apple_size) {
      x = apple[X] + dx;
      y = apple[Y] + dy;
      *screen_xy(x, y) = APPLE;
      dx += 1;
    }
    dy += 1;
  }
  *screen_xy(x, y) = APPLE;
  render();
}

update() {
  extrn memset, memmove, memcpy;

  memmove(body_i(1), body, (body_len-1)*W2);
  memcpy(body, head, W2);

  // need ternary operator
  if (facing == UP) {
    head[Y] = head[Y] - 1;
    if (head[Y] < 0) head[Y] += height;
  } else if (facing == DOWN){
    head[Y] = (head[Y]+1) % height;
  } else if (facing == RIGHT){
    head[X] = (head[X]+1) % width;
  } else if (facing == LEFT){
    head[X] = head[X] - 1;
    if (head[X] < 0) head[X] += width;
  } else {
    unreachable("in update(), autovar `facing` containt unknown direction variant");
  }

  if (is_colliding_with_apple(head)) {
    body_len += 3;
    score += 1;
    randomize_apple();
  }

  auto i; i = 0; while (i < body_len) {
    auto segment;
    segment = body_i(i);
    dead |= head[X]==segment[X] & head[Y]==segment[Y];
    i += 1;
  }

  if (dead) {
    memcpy(head, body, W2);
    just_died = 1;
  }

  // !when division
  frame_time = 80 - (body_len>>2);
}

main() {
  extrn usleep;
  init_globals();
  randomize_apple();
  enable_raw_mode();

  auto exit; exit = 0; while (!exit) {
    draw_screen();
    usleep(frame_time*1000);
    exit = handle_user_input();
    if (!dead) update();
    frame += 1;
  }

  disable_raw_mode();
}

// TODO: does not work on gas-x86_64-windows due to using POSIX stuff
//   Would be nice to research how hard/easy it is to add the Window support.
//   But this is not critical.
