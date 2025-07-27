global_variable 0;
main() {
    __asm__ (
        "mov &global_variable, r0",
        "mov x69, r1",
        "mov.l r0, @r1"
    );
    return (global_variable);
}
