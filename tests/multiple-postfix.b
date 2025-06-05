main() {
    extrn printf, malloc;
    auto arr, W;
    W = &0[1];

    arr    = malloc(2 * W);
    arr[0] = malloc(2 * W);
    arr[1] = malloc(2 * W);
    arr[0][0] = 34;
    arr[0][1] = 35;
    arr[1][0] = 69;
    arr[1][1] = 420;

    arr[1][0]++;
    --arr[1][1];

    printf("%d  %d\n%d %d\n",
           arr[0][0],
           arr[0][1],
           arr[1][0],
           arr[1][1]);
}
