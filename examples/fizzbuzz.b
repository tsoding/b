main() {
    extrn printf;

    auto i;
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
}
