div(a, b) {
    extrn printf;
    printf("%d/%d = %d\n", a, b, a/b);
}

mod(a, b) {
    extrn printf;
    printf("%d%%%d = %d\n", a, b, a%b);
}

main() {
    extrn printf;
    printf("Division:\n");
    div(   1, 100);
    div(  -1, 100);
    div( 100, 100);
    div(-100, 100);
    div( 101, 100);
    div(-101, 100);
    div( 201, 100);
    div(-201, 100);
    printf("\n");
    printf("Remainder:\n");
    mod(  1, 100);
    mod( 99, 100);
    mod(100, 100);
    mod(101, 100);
    mod(201, 100);
    mod( -1, 100);
}
