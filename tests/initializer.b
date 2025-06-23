returns7() {
	return (7);
}

main() {
	extrn printf;
	auto x = 5;
	auto y = returns7() * 15;
	printf("x: %d, y: %d\n", x, y);
}
