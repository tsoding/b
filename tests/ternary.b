printn(n) {
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
	printn(0);
	printn(42);
	printn(69);
	printn(96);
	printn(420);
	printn(690);
	printn(1337);
	printn(42069);
}
