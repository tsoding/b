main() {
    extrn printf;
    extrn exit;

    auto i;

    i = 0; while (i < (6 / 2)) {
        printf("=");
        i++;
    }
    printf("\n");

    i = 1;

    while (i <= 100) {
        if (i % 3 == 0 | i % 5 == 0) {
            if (i % 3 == 0) printf("Fizz");
            if (i % 5 == 0) printf("Buzz");
        } else {
            printf("%d", i);
        }

        printf("\n");
        i++;
    }
    return;
}
