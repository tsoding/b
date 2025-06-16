foo 0x0102030405060708;

main() {
    extrn assert_equal;
    assert_equal(foo, 0x0102030405060708, "foo == 0x0102030405060708");
}
