g(i, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) {
    if(i <= 0) {
    	return (a10 + 11);
    }
    return (g(i-1, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11));
}

f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) {
    return (a2 + g(4, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11));
}

main() {
    extrn printf;
    printf("Testing how well passing 11 arguments to a function we defined works.\n");
    printf("Expected output is `23`\n");
    printf("%d\n", f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11));
}
