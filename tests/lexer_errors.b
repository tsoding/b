// TODO: Enable in test suite once negative testing is supported.

main() {
    // Invalid character literals
    '';
    'EEE';
    
    // Literal overflow
    0xfffffffffffffffffffff;
    07777777777777777777777;

    // Unknown escape sequences
    "\foo\bar\baz";
}
