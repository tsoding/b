main() {
    extrn printf;
    auto xs 4, W;
    W = &0[1]; // word size

    *xs = 34;
    *(xs + 1*W) = '+';
    xs[2] = 35;
    xs[3] = 69;

    printf(
        "%d %c %d = %d\n",
        xs[0],
        xs[1],
        *(xs + 2*W),
        *(xs + 3*W)
    );
}
