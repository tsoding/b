str "string1\n";
integer 4;

strings [4]
    str,
    "string2\n",
    "string3\n",
    "string4\n";

integers [4]
    0,
    2,
    integer,
    6;

main() {
    extrn printf, putchar, char;

    auto i;
    i = 0;

    while (i < 4) {
        printf(strings[i]);
        i++;
    }

    i = 0;
    while (i < 4) {
        putchar(char(str, integers[i]));
        i++;
    }

    putchar('\n');
}