// Originally taken from https://github.com/tsoding/b/pull/173
// After https://github.com/tsoding/b/pull/197 we do not expect it to fail anymore.
main() {
    extrn printf;
    auto x,y,z;
    x = 9223372036854775808;
    y = 0x8000000000000000;
    z = 01000000000000000000000;
    printf("x = %llX\n", x);
    printf("y = %llX\n", y);
    printf("z = %llX\n", z);
}
