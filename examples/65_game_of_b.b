// To compile this example you need to pass appropriate linker flags to the b compiler:
// $ b 65_game_of_b.b -L -lncurses -L -lpanel -run

TRUE;
FALSE;
ALIVE_CHAR;
LOOP_TIME_us;
NUM_SPEEDS;

x_size;
y_size;
x_chars;
y_chars;
buf_size;
cur_buf;
prv_buf;
world_win;
info_panel;
speeds;

int32(array, i) {
    extrn memcpy;
    auto val;
    val = 0;
    memcpy(&val, array + (i * 4), 4);
    return (val);
}

init_ncurses() {
    extrn initscr, noecho, cbreak, curs_set, timeout, keypad, stdscr, mousemask, mouseinterval;
    initscr();
    noecho();
    cbreak();
    curs_set(0);
    timeout(0);
    keypad(stdscr, 1);
    mousemask(1, 0); // BUTTON1_RELEASED
    mouseinterval(10);
}

deinit_ncurses() {
    extrn endwin, echo, curs_set;
    endwin();
    echo();
    curs_set(1);
}

init_globals() {
    TRUE = 1;
    FALSE = 0;
    ALIVE_CHAR = 'B';
    LOOP_TIME_us = 10000;
    NUM_SPEEDS = 16;

    extrn getmaxx, getmaxy, stdscr, malloc, lchar, char;
    x_size = getmaxx(stdscr) / 2;
    y_size = getmaxy(stdscr);
    x_chars = x_size * 2;
    y_chars = y_chars;
    buf_size = x_size * y_size;
    cur_buf = malloc(buf_size);
    prv_buf = malloc(buf_size);

    speeds = malloc(NUM_SPEEDS);
    lchar(speeds, 0, 1);
    auto prv_speed;
    auto i; i = 1; while(i < NUM_SPEEDS) {
        prv_speed = char(speeds, i-1);
        lchar(speeds, i, prv_speed + (prv_speed / 3) + 1);
        i++;
    }
}

init_world() {
    extrn newwin, new_panel;
    auto world_panel;
    world_win = newwin(y_chars, x_chars, 0, 0);
    world_panel = new_panel(world_win);

    extrn start_color, init_pair, wattron, COLOR_PAIR;
    start_color();
    init_pair(42, 7, 1); // foreground -> COLOR_WHITE, background -> COLOR_RED
    wattron(world_win, COLOR_PAIR(42));
}

init_info() {
    auto x_info_chars, y_info_chars;
    x_info_chars = 20;
    y_info_chars = 9;

    extrn newwin, new_panel;
    auto info_win;
    info_win = newwin(y_info_chars, x_info_chars, 0, x_chars - x_info_chars - 1);
    info_panel = new_panel(info_win);

    extrn box, mvwaddstr;
    box(info_win, 0, 0);
    mvwaddstr(info_win, 1, 2, "i -> toggle info");
    mvwaddstr(info_win, 2, 2, "s -> start/stop");
    mvwaddstr(info_win, 3, 2, "k -> speed up");
    mvwaddstr(info_win, 4, 2, "j -> slow down");
    mvwaddstr(info_win, 5, 2, "c -> clear world");
    mvwaddstr(info_win, 6, 2, "r -> reset world");
    mvwaddstr(info_win, 7, 2, "q -> quit game");
}

is_alive(buf, y, x) {
    extrn lchar, char;
    return (char(buf, (y * x_size) + x) != 0);
}

set_alive(buf, y, x, alive) {
    extrn lchar, char;
    lchar(buf, (y * x_size) + x, alive);
}

clear_world() {
    extrn memset;
    memset(cur_buf, 0, buf_size);
}

reset_world() {
    auto x_mid, y_mid;
    x_mid = x_size / 2;
    y_mid = y_size / 2;
    clear_world();
    set_alive(cur_buf, y_mid-1, x_mid+0, TRUE);
    set_alive(cur_buf, y_mid+0, x_mid+1, TRUE);
    set_alive(cur_buf, y_mid+1, x_mid-1, TRUE);
    set_alive(cur_buf, y_mid+1, x_mid+0, TRUE);
    set_alive(cur_buf, y_mid+1, x_mid+1, TRUE);
}

update_world() {
    auto tmp_buf;
    tmp_buf = prv_buf;
    prv_buf = cur_buf;
    cur_buf = tmp_buf;
    clear_world();
    auto y; y = 0; while(y < y_size) {
        auto x; x = 0; while(x < x_size) {
            auto up, down, left, right;
            up = (y + y_size - 1) % y_size;
            down = (y + 1) % y_size;
            left = (x + x_size - 1) % x_size;
            right = (x + 1) % x_size;
            auto self, other;
            self = is_alive(prv_buf, y, x);
            other = is_alive(prv_buf, up, left)
                  + is_alive(prv_buf, up, x)
                  + is_alive(prv_buf, up, right)
                  + is_alive(prv_buf, y, left)
                  + is_alive(prv_buf, y, right)
                  + is_alive(prv_buf, down, left)
                  + is_alive(prv_buf, down, x)
                  + is_alive(prv_buf, down, right);
            if(self) {
                if(other == 2 | other == 3) {
                    set_alive(cur_buf, y, x, TRUE);
                }
            } else {
                if(other == 3) {
                    set_alive(cur_buf, y, x, TRUE);
                }
            }
            x++;
        }
        y++;
    }
}

print_world() {
    extrn werase, wmove, waddch, update_panels, doupdate;
    werase(world_win);
    auto y; y = 0; while(y < y_size) {
        auto x; x = 0; while(x < x_size) {
            if(is_alive(cur_buf, y, x)) {
                wmove(world_win, y, x*2);
                waddch(world_win, ALIVE_CHAR);
                waddch(world_win, ALIVE_CHAR);
            }
            x++;
        }
        y++;
    }
    update_panels();
    doupdate();
}

main() {
    init_ncurses();
    init_globals();
    init_world();
    init_info();
    reset_world();

    extrn malloc, usleep, getch, show_panel, hide_panel, getmouse, char;
    auto input, redraw, update_in, show_info, stopped, speed_index, mouse_event;
    input = -1;
    redraw = 1;
    update_in = 0;
    show_info = 1;
    stopped = 0;
    speed_index = 4;
    mouse_event = malloc(20); // MEVENT
    while(input != 'q') {
        if(redraw) {
            print_world();
            redraw = 0;
        }
        usleep(LOOP_TIME_us);
        if(!stopped) {
            update_in--;
            if(update_in <= 0) {
                update_world();
                redraw = 1;
                update_in = char(speeds, speed_index);
            }
        }
        input = getch();
        if(input == 'i') {
            show_info = !show_info;
            if(show_info) {
                show_panel(info_panel);
            } else {
                hide_panel(info_panel);
            }
            redraw = 1;
        } else if(input == 's') {
            stopped = !stopped;
        } else if(input == 'k') {
            speed_index--;
            if(speed_index < 0) speed_index = 0;
        } else if(input == 'j') {
            speed_index++;
            if(speed_index >= NUM_SPEEDS) speed_index = NUM_SPEEDS - 1;
        } else if(input == 'c') {
            clear_world();
            redraw = 1;
        } else if(input == 'r') {
            reset_world();
            redraw = 1;
        } else if(input == 0x199) { // KEY_MOUSE
            if (getmouse(mouse_event) == 0) { // OK
                auto x, y;
                x = int32(mouse_event, 1) / 2;
                y = int32(mouse_event, 2);
                set_alive(cur_buf, y, x, !is_alive(cur_buf, y, x));
                redraw = 1;
            }
        }
    }
    deinit_ncurses();
}
