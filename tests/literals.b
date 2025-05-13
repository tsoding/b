main() {
    extrn printf;
    auto fmt;
    fmt = "%lu\n"; // TODO: hack because AArch64 does not support several string literals. inline when it does.
    printf(fmt, 69);
    printf(fmt, 1000000);
    printf(fmt, 123456789987654321);
}
