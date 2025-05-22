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
// (22/05/2025 09:02)
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
STDIN;
STDOUT;
TCSAFLUSH;
FIONREAD;
TIOCGWINSZ;

// enum tile
EMPTY;
BODY;
HEAD;
APPLE;

// enum facing
UP;
RIGHT;
DOWN;
LEFT;

// state
stop;
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


// poor man's structs
pos; pos2; pos3; x; y; pos_eq; offset;
pos_get() { x = *pos; y = *(pos+W); }
pos_set() { *pos = x; *(pos+W) = y; }
pos_offset() { pos += offset*W2; }
pos2_offset() { pos2 += offset*W2; }
pos_cmp() {
  auto x1, y1;
  pos_get();
  x1 = x;
  y1 = y;
  pos3 = pos;
  pos = pos2;
  pos_get();
  pos = pos3;
  pos_eq = x==x1 & y==y1;
}


is_colliding_with_apple;
check_apple_collision() {
  pos2 = pos;
  pos = apple; pos_get();
  auto ox, oy; ox = x; oy = y;
  is_colliding_with_apple = 0;
  auto i; i=0; while (i < apple_size) {
    auto j; j=0; while (j < apple_size) {
      x = ox + i;
      y = oy + j;
      pos_set(); pos_cmp();
      is_colliding_with_apple |= pos_eq;
      j += 1;
    }
    i += 1;
  }
  x = ox; y = oy; pos_set();
}

randomize_apple() {
  extrn rand;
  auto again; again = 1;
  while (again) {
    x = rand() % (width - (apple_size+2))+1;
    y = rand() % (height - (apple_size+2))+1;
    pos = apple; pos_set();
    pos = head; check_apple_collision();
    again = is_colliding_with_apple;

    auto i; i = 0; while (i < body_len) {
      pos = body; offset = i; pos_offset();
      check_apple_collision();
      again |= is_colliding_with_apple;
      i += 1;
    }
  }
}

init_globals() {
  extrn printf, malloc, memset, getchar, read, fflush, usleep, ioctl, exit, srand, time;
  srand(time(0));

  W = 8;
  W2 = W*2;
  STDIN = 0;
  STDOUT = 1;
  TCSAFLUSH = 2;
  TIOCGWINSZ = 21523;
  FIONREAD = 21531;

  auto ws;
  ws = malloc(W*2);
  ioctl(STDOUT, TIOCGWINSZ, ws);

  height = (*ws & 0xffff)-3;
  width = (*ws >> 17 & 0x7fff)-2;
  apple_size = 3;
  dead = 0;
  frame = 0;


  EMPTY = 0;
  BODY = 1;
  HEAD = 2;
  APPLE = 4;

  UP = 0;
  RIGHT = 1;
  DOWN = 2;
  LEFT = 3;

  stop = 0;
  screen_size = width*height*W;
  screen = malloc(screen_size);
  memset(screen, EMPTY, screen_size);

  // add `&` to take address of variable
  // instead of spamming malloc everywhere
  input_ch = malloc(W);
  bytes_remaining = malloc(W);

  head = malloc(W2);
  pos = head; x = width>>1; y = height>>1; pos_set();

  body = malloc(screen_size*W2);
  memset(body, 255, screen_size);
  body_len = 5;
  score = 0;

  apple = malloc(screen_size*W2);
  randomize_apple();

  frame_time = 100;
  facing = RIGHT;
}

unreachable_message;
unreachable() {
  extrn printf, abort;
  printf("\nUNREACHABLE: %s\n", unreachable_message);
  abort();
}

render() {
  // I'd love to use putchar put character literals are not implemented
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
      tile = *(screen + (x+y*width)*W);
      if (tile == EMPTY) {
        printf("%c[48;5;233m  ", 27);
      } else if (tile == BODY) {
        if (dead) printf("%c[38;5;245m██", 27);
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
        if (dead) printf("%c[48;5;245m", 27);
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
          unreachable_message = "render head direction check";
          unreachable();
        }
      } else if (tile == APPLE) {
        // auto color;
        // color = (frame*2) % 64;
        // if (color < 6) color = 196 + color;
        // else if (color < 12) color = 207 - color;
        // else color = 196;
        // printf("%c[38;5;%dm██", 27, color);
        printf("%c[38;5;196m██", 27);
      } else {
        unreachable_message = "render tile type check";
        unreachable();
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
  extrn malloc, tcgetattr, tcsetattr, printf, memcpy, memset;
  orig_termios = malloc(64);
  tcgetattr(STDIN, orig_termios);

  auto raw;
  raw = malloc(64);
  *raw = *orig_termios & 0xfffffff5;
  memcpy(raw+4, orig_termios+4, 4); // HOW TO SET ONLY 32-BITS ??

  tcsetattr(STDIN, TCSAFLUSH, raw);
  printf("%c[?25l", 27);
}

disable_raw_mode() {
  extrn tcsetattr, printf;
  tcsetattr(STDIN, TCSAFLUSH, orig_termios);
  printf("%c[?25h", 27);
}

handle_user_input() {
  extrn read, ioctl;
  auto f;
  f = facing;
  ioctl(STDIN, FIONREAD, bytes_remaining);
  while (*bytes_remaining != 0) {
    read(STDIN, input_ch, 1);
    if (*input_ch == 113) { stop = 1; } // q
    if (!dead) {
      if (*input_ch == 119 & f != DOWN)  { facing = UP;    } // w
      if (*input_ch == 97  & f != RIGHT) { facing = LEFT; } // a
      if (*input_ch == 115 & f != UP)    { facing = DOWN;  } // s
      if (*input_ch == 100 & f != LEFT)  { facing = RIGHT;  } // d
      if (*input_ch == 114) { rgb = !rgb; } // r
    } else {
      init_globals();
    }
    ioctl(STDIN, FIONREAD, bytes_remaining);
  }
}

draw_screen() {
  extrn memset;
  memset(screen, EMPTY, screen_size);
  auto i, j;
  i = 0;
  while (i < body_len) {
    pos = body; offset = i; pos_offset(); pos_get();
    *(screen + (x+y*width)*W) = BODY;
    i += 1;
  }

  pos = head; pos_get();
  *(screen + (x+y*width)*W) = HEAD;

  pos = apple; pos_get();
  i = 0; while (i < apple_size) {
    j = 0; while (j < apple_size) {
      *(screen + ((x+i)+(y+j)*width)*W) = APPLE;
      j += 1;
    }
    i += 1;
  }
  *(screen + (x+y*width)*W) = APPLE;
  render();
}

update() {
  extrn memset;

  extrn memmove, memcpy;
  memmove(body+W2, body, (body_len-1)*W2);
  memcpy(body, head, W2); 

  // need -=
  pos = head; pos_get();
  if (facing == UP) {
    if (y == 0) y = height - 1;
    else y = y - 1;
  } else if (facing == DOWN){
    y += 1;
    y = y % height;
  } else if (facing == RIGHT){
    x += 1;
    x = x % width;
  } else if (facing == LEFT){
    if (x == 0) x = width - 1;
    else x = x - 1;
  } else {
    unreachable_message = "update facing direction check";
    unreachable();
  }
  pos_set();


  pos = head; check_apple_collision();
  if (is_colliding_with_apple) {
    body_len += 3;
    score += 1;
    randomize_apple();
  }

  auto i; i = 0; while (i < body_len) {
    pos = body; offset = i; pos_offset();
    pos2 = head; pos_cmp();
    dead |= pos_eq;
    i += 1;
  }

  if (dead) {
    pos = body;
    pos_get();
    pos = head;
    pos_set();
  }

  // !when division
  frame_time = 80 - (body_len>>2);
}

main() {
  extrn usleep;
  init_globals();
  randomize_apple();
  enable_raw_mode();

  // having `break` would be nice
  // also `true` and `false`
  while (!stop) {
    draw_screen();
    usleep(frame_time*1000);
    handle_user_input();
    if (!dead) update();
    frame += 1;
  }

  disable_raw_mode();
}
