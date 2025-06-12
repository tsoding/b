/* Based on exanples/60_game_of_life.b */
width;
height;
W;
board1;
board2;
board;
next;
x;
y;
r;

mod(n, b) return ((n%b + b)%b);

get(b, xn, yn) {
	xn = mod(xn, width);
	yn = mod(yn, height);
	return (b[xn+yn*width]);
}
set(b, xn, yn, v) {
	xn = mod(xn, width);
	yn = mod(yn, height);
	b[xn+yn*width] = v;
}


print() {
	extrn printf;
	auto xn, yn;

	xn = 0; while (xn <= width) {
		printf("##");
		xn += 1;
	}
	printf("\n");

	yn = 0; while (yn < height) {
		printf("#");
		xn = 0; while (xn < width) {
			printf(get(*board, xn,yn) ? "██" : "  ");
			xn += 1;
		}
		printf("#\n");
		yn += 1;
	}

	xn = 0; while (xn <= width) {
		printf("##");
		xn += 1;
	}
	printf("\n");
}

step() {
	extrn printf;

	if (get(*board, x, y)) r++; else r--;

	set(*board, x, y, !get(*board, x, y));
	switch mod(r, 4) {
	case 0: y++; goto out;
	case 1: x++; goto out;
	case 2: y--; goto out;
	case 3: x--; goto out;
	}
out:

	set(*board, x, y, !get(*board, x, y));
	auto tmp;
	tmp = board;
	board = next;
	next = tmp;
}

main() {
	extrn malloc, memset, printf, usleep;
	auto size;
	width  = 25;
	height = 15;
	size = width*height*(&0[1]);

	board1 = malloc(size);
	board2 = malloc(size);
	memset(board1, 0, size);
	memset(board2,  0, size);
	board = &board1;
	next = &board2;

	r = 0;
	x = 15;
	y = 7;

	while (1) {
		print();
		step();
		printf("%c[%dA", 27, height+2);
        // TODO: does not work on Windows
		usleep(1500);
	}
}
