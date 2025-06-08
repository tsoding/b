// Prompted by https://github.com/tsoding/b/pull/95
test_main() {
    auto a;
    a = 1 ? 69 : 420;
    extrn assert_equal;
    assert_equal(a, 69, "a = 1 ? 69 : 420; a == 69");
}
