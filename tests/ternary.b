test(n) {
	extrn printf;
	printf(
		"%d:\t%s\n", n,
		n == 69 ? "69" :
		n == 420 ? "420" :
		n < 69 ? "..69" :
		n >= 420 ?
			n >= 1337 & n != 1337 ? "1337.." :
	   "420..=1337" :
		"69..420"
	);
}
main(argc, argv) {
	test(0);
	test(42);
	test(69);
	test(96);
	test(420);
	test(690);
	test(1337);
	test(4269);
}
