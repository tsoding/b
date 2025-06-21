foo 0x0102030405060708;

assert_equal(actual, expected, message) {
    extrn printf, abort;
    printf("%s: ", message);
    if (actual != expected) {
        printf("FAIL\n");
        abort();
    } else {
        printf("OK\n");
    }
}

main() {
    extrn assert_equal;
    assert_equal(foo, 0x0102030405060708, "foo == 0x0102030405060708");
}
