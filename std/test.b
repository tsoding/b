// Utilities for testing.

errors;

assert_equal(actual, expected, message) {
    extrn printf, abort;
    printf("%s: ", message);
    if (actual != expected) {
        printf("FAIL\n");
	errors++;	
    } else {
        printf("OK\n");
    }
}

main() {
	extrn printf;
	errors = 0;
	extrn test_main;
	test_main();

	printf("Failed with %d errors.\n", errors);
	return ((errors == 0) ? 0 : 1);
}
