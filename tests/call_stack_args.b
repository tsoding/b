f(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) {
    extrn printf;
    printf("%d %d %d %d %d %d %d %d %d %d\n", a0, a1, a2, a3, a4, a5, a6, a7, a11, a10);
    printf("%d %d %d %d %d %d %d %d %d %d %d\n", a0, a1, a2, a3, a4, a5, a6, a7, a11, a10, a9);
    printf("%d %d %d %d %d %d %d %d %d %d %d %d\n", a0, a1, a2, a3, a4, a5, a6, a7, a11, a10, a9, a8);
}

main() {
    extrn printf;
    f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
}
