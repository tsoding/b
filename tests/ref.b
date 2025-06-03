write(ref, val) *ref = val;
read(ref) return (*ref);

y;
test1() {
	extrn printf;
	auto x;

	write(&x, 69);
	write(&y, 420);

	// printf("&x: %p\n", &x);
	// printf("&y: %p\n", &y);

	printf("x: %d %d %d %d %d\n", x, *&x, &*x, &*&*&*x, *&*&*&x);
	printf("y: %d %d %d %d %d\n", y, *&y, &*y, &*&*&*y, *&*&*&y);
}

test2() {
	extrn printf;
	auto a, b, c, d;
	a = 1337;
	b = &a;
	c = &b;
	d = &c;
	printf("a: %d\n", ***d);
}

test3() {
	extrn printf, malloc;
	auto xs;

	xs = malloc(8*2);
	xs[0*8] = 13;
	xs[1*8] = 42;

	// "E1[E2] is identical to *(E1+E2)"
	// "&*x is identically x"
	// therefore `&E1[E2]` should be identical to `E1+E2`
	// check generated IR to confirm that
	printf(
		"xs: [%d, %d]\n",
		read(xs + 0*8),
		read(&xs[1*8])
	);
}

main() {
	test1();
	test2();
	test3();
}
