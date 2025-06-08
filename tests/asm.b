// TODO: this test is too platform specific to be run on `make test`
//   How should we approach dealing with such tests?
//   Maybe on top of having a list of common tests we should have the list of
//   very platform specific tests?
test_main() {
    __asm__(
    "mov rax, 69",
    "mov rsp, rbp",
    "pop rbp",
    "ret"
    );
}
