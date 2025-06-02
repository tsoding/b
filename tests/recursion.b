func(i) {
	extrn printf;
	if (i > 0) {
		printf("%d\n", i);
		func(i-1);
	}
}

main() func(10);
