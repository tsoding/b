main() {
    extrn printf, execvp, sx64;
    auto x;
    x = execvp("executable_that_does_not_exist", 0);
    x = sx64(x);
    if (x < 0) {
        printf("OK\n");
    } else {
        printf("FAILURE\n");
    }
}
