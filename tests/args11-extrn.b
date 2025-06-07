main() {
    extrn printf;
    printf("Testing how well passing 11 arguments works.\n");
    printf("Expected output is `1 2 3 4 5 6 7 8 9 10`\n");
    printf("%d %d %d %d %d %d %d %d %d %d\n", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
}
