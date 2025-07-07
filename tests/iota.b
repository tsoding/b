STRUCT_OKINA_FOO iota;
STRUCT_OKINA_BAR iota;
STRUCT_OKINA_BAZ iota;

/* Try to reset an iota to start a new struct */
reset;
STRUCT_YUKARI_BAR iota;
STRUCT_YUKARI_FOO iota;
STRUCT_YUKARI_BAZ iota;

list[4];

main() {
    reset;

    printf("%d %d %d\n", STRUCT_OKINA_FOO, STRUCT_OKINA_BAR, STRUCT_OKINA_BAZ);
    printf("%d %d %d\n", STRUCT_YUKARI_FOO, STRUCT_YUKARI_BAR, STRUCT_YUKARI_BAZ);
    
    /* Using iotas in general expressions */
    list[iota] = 1;
    list[iota] = 2;
    list[iota] = 3;
    list[iota] = 4;
    printf("%d %d\n", iota, list[1]);

    reset;
    printf("%d\n", iota);
    return (0);
}
