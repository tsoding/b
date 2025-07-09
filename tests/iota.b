STRUCT_OKINA_FOO iota_set(0);
STRUCT_OKINA_BAR iota_skip(1);
/* iota_skip can generally be used to create an n-element wordlist */
STRUCT_OKINA_BAZ iota_skip(12);

/* Try to reset an iota to start a new struct */
STRUCT_YUKARI_BAR iota_reset;
STRUCT_YUKARI_FOO iota;
STRUCT_YUKARI_BAZ iota_skip(128);
STRUCT_YUKARI_BARRIER iota;

list[4];

main() {
    printf("%d %d %d\n", STRUCT_OKINA_FOO, STRUCT_OKINA_BAR, STRUCT_OKINA_BAZ);
    printf("%d %d %d\n", STRUCT_YUKARI_FOO, STRUCT_YUKARI_BAR, STRUCT_YUKARI_BAZ);
    
    /* Using iotas in general expressions */
    list[iota_reset] = 1;
    list[iota] = 2;
    list[iota] = 3;
    list[iota] = 4;
    printf("%d %d\n", iota, list[1]);

    /* iota = N <=> iota_set(N) */
    printf("%d\n", iota = 0);
    return (0);
}
