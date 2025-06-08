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
	
	if (get(*board, x, y) == 1) {
		r++;
	} else {
		r--;
	}

	if (r > 3) {
		r = 0;
	} else if (r < 0){
		r = 3;
	}

	set(*board, x, y, !get(*board, x, y));
	if (r == 0) {
		y++;
	} else if (r == 1) {
		x++;
	} else if (r == 2) {
		y--;
	} else if (r == 3) {
		x--;
	}



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
