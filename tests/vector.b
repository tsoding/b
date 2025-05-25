main() {
    extrn printf, malloc;
    auto xs, W;
    W = 8; // word size
    xs = malloc(4*W);

    *xs = 34;
    *(xs + 1*W) = '+';
    xs[2*W] = 35;
    (3*W)[xs] = 69;

    printf(
        "%d %c %d = %d\n",
        0[xs],
        xs[1*W],
        *(xs + 2*W),
        *(xs + 3*W)
    );
}
