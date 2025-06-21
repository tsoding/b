// Prompted by https://github.com/tsoding/b/pull/95
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
    auto a;
    a = 1 ? 69 : 420;
    extrn assert_equal;
    assert_equal(a, 69, "a = 1 ? 69 : 420; a == 69");
}
