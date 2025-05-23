main() {
    extrn printf, malloc;
    auto v;
    v = malloc(8);

    *v =   1;    printf("*v =   1    v=%d\n", *v);
    *v |=  16;   printf("*v |=  16   v=%d\n", *v);
    *v *=  2;    printf("*v *=  2    v=%d\n", *v);
    *v +=  35;   printf("*v +=  35   v=%d\n", *v);
    *v <<= 1;    printf("*v <<= 1    v=%d\n", *v);
    *v &=  127;  printf("*v &=  127  v=%d\n", *v);
}