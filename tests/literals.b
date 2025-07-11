// TODO: Literals this large _will_ fail on some platforms with a low word size
main() {
    extrn printf;
    auto fmt;
    // This must be `llu` and not `lu` because on windows `long` is 32-bits
    printf("%llu\n", 69);
    printf("%llu\n", 1000000);
    printf("%llu\n", 123456789987654321);
}
