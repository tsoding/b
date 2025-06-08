func(i) {
	extrn printf;
	if (i > 0) {
		printf("%d\n", i);
		func(i-1);
	}
}

test_main() func(10);
