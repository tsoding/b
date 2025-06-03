main() {
    extrn printf, malloc;
    auto arr, W;
    W = 8;

    arr = malloc(2 * W);
    arr[0] = malloc(2 * W);
    arr[W] = malloc(2 * W);
    arr[0][0] = 34;
    arr[0][W] = 35;
    arr[W][0] = 69;
    arr[W][W] = 420;

    arr[W][0]++;
    --arr[W][W];

    printf("%d  %d\n%d %d\n",
           arr[0][0],
           arr[0][W],
           arr[W][0],
           arr[W][W]);
}
