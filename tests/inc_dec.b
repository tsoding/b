main() {
	extrn printf;
	auto x;
	x = 3;
	printf("x: %d\n", x);
	printf("++x: %d\n", ++x);
	printf("x++: %d\n", x++);
	printf("x: %d\n", x);
	printf("x--: %d\n", x--);
	printf("--x: %d\n", --x);
}
