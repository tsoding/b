/* B comment */
// C++ comment

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
    assert_equal(0105, 69, "0105 == 69");
    assert_equal(0x45, 69, "0x45 == 69");
    assert_equal('E', 0x45, "'E' == 0x45");
    assert_equal('EF', 0x4546, "'EF' == 0x4546");
}
