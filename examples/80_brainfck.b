// Copyright (c) 2025 luxluth <delphin.blehoussi93@gmail.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.


STDIN;
BUF_LEN;
MEMORY_LEN;

main() {
  extrn malloc, memset, read, printf, getchar, atoi, fflush;
  auto memory, cursor, len, input_buf, stop, addr_buf, W;

  MEMORY_LEN = 30000; STDIN = 0; BUF_LEN = 512;

  W      = &0[1];
  cursor = 0;
  stop   = 0;

  memory    = malloc(MEMORY_LEN*W);
  addr_buf  = malloc(5);
  input_buf = malloc(BUF_LEN);

  memset(memory, 0, MEMORY_LEN*W);
  memset(addr_buf, 0, 5);
  memset(input_buf, 0, BUF_LEN);

  while (!stop) {
    auto cmdslen; cmdslen = 0;
    auto input_cursor; input_cursor = 0;

    printf("# ");
    fflush(0);
    cmdslen = read(STDIN, input_buf, BUF_LEN);
    if (cmdslen > 0) {
      *(input_buf + cmdslen - 1) = 0; // removing the extra newline
      cmdslen -= 1;
    } else {
      stop = 1;
    }

    while(input_cursor < cmdslen) {
      auto cmd, fullfilled; fullfilled = 0;
      cmd = *(input_buf + input_cursor) & 0xFF;

      if ((!fullfilled) & (cmd == '>')) {
        if (cursor < MEMORY_LEN) cursor += 1;
        fullfilled = 1;
      }
      if ((!fullfilled) & (cmd == '<')) {
        if (cursor > 0) cursor -= 1;
        fullfilled = 1;
      }
      if ((!fullfilled) & (cmd == '+')) {
        if (memory[cursor] == 255) {
          memory[cursor] = 0;
        } else {
          memory[cursor] = memory[cursor] + 1;
        }
        fullfilled = 1;
      }
      if ((!fullfilled) & (cmd == '-')) {
        if (!memory[cursor]) {
          memory[cursor] = 255;
        } else {
          memory[cursor] = memory[cursor] - 1;
        }
        fullfilled = 1;
      }
      if ((!fullfilled) & (cmd == '.')) {
        printf("%c", memory[cursor]);
        fflush(0);
        fullfilled = 1;
      }

      if (cmd == ',') {
        auto char_val; char_val = getchar();
        memory[cursor] = char_val;
        fullfilled = 1;
      }

      if (cmd == '[') {
        if (!memory[cursor]) {
          auto stack, jumped;
          stack = 1; jumped = 0;
          input_cursor += 1;

          while((!jumped) & (input_cursor < cmdslen)) {
            cmd = *(input_buf + input_cursor) & 0xFF;
            if (cmd == '[') {
              stack += 1;
            }
            if (cmd == ']') {
              stack -= 1;
              if (stack == 0) {
                jumped = 1;
                input_cursor -= 1;
              }
            }

            input_cursor += 1;
          }
        }

        fullfilled = 1;
      }


      if ((!fullfilled) & (cmd == ']')) {
        if (memory[cursor]) {
          auto stack, jumped;
          stack = 1; jumped = 0;
          input_cursor -= 1;

          while((!jumped) & (input_cursor >= 0)) {
            cmd = *(input_buf + input_cursor) & 0xFF;

            if (cmd == ']') {
              stack += 1;
            }
            if (cmd == '[') {
              stack -= 1;
              if (stack == 0) {
                jumped = 1;
              }
            }

            input_cursor -= 1;
          }
        }
        fullfilled = 1;
      }
      if ((!fullfilled) & (cmd == '#')) {
        cursor = 0;
        memset(memory, 0, MEMORY_LEN*W);
      }

      if ((!fullfilled) & (cmd == '$') & (cmdslen == 1)) {
        printf("MEMORY ADDRESS (0-29999): ");
        auto addr_len, addr; addr_len = read(STDIN, addr_buf, 5);
        if (addr_len > 0) {
          *(addr_buf + addr_len - 1) = 0;
          addr = atoi(addr_buf);
          printf("MEMORY(+%d) -> %d\n", addr, memory[addr]);
        }
      }

      input_cursor += 1;
    }

    printf("\n");
  }
}

// Classic Hello world
// >+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++.------.--------.[-]>++++++++[<++++>- ]<+.[-]++++++++++.

// Special commands:
// - # -> to reset the memory and the cursor
// - $ -> to inspect a memory address
