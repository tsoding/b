main() {
    extrn printf;
    auto fmt;
    // This must be `llu` and not `lu` because on windows `long` is 32-bits
    fmt = "%llu\n"; // TODO: hack because AArch64 does not support several string literals. inline when it does.
    printf(fmt, 69);
    printf(fmt, 1000000);
    printf(fmt, 123456789987654321);
}
