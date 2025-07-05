// Written by LDA (seija-amanojaku)
// ---------------------------------
// Pretty much just a truth machine, as specified by https://esolangs.org/wiki/Truth-machine

main() {
    extrn printf, getchar;
    auto input;

    while (1) {
        printf("Input?\n");
        input = getchar();
        if (input == '0') {
            printf("0\n");
            return (0);
        } else if (input == '1') {
            while (1) printf("1\n");
        }
    }
    return (1);
}
