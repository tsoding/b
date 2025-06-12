/* https://en.wikipedia.org/wiki/Langton%27s_ant */
width;
height;
board;
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
			if ((xn == mod(x, width)) & (yn == mod(y, height))) {
				printf("▒▒");
			} else {
				if (get(board, xn,yn)) {
					printf("██");
				} else {
					printf("  ");
				}
			}
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
	auto c;
	c = get(board, x, y);
	if (c) r++; else r--;
	set(board, x, y, !c);
	switch mod(r, 4) {
	case 0: y++; goto out;
	case 1: x++; goto out;
	case 2: y--; goto out;
	case 3: x--; goto out;
	}
out:
}

main() {
	extrn malloc, memset, printf, usleep;
	auto size;
	width  = 25;
	height = 15;
	size = width*height*(&0[1]);

	board = malloc(size);
	memset(board, 0, size);

	r = 0;
	x = 15;
	y = 7;

	while (1) {
		print();
		step();
		printf("%c[%dA", 27, height+2);
		/* TODO: does not work on Windows */
		usleep(50000);
	}
}
