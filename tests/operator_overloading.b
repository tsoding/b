f1(a, b) {
    printf("1: %d * %d\n", a, b);
    return (a+b);
}
f2(a, b) {
    printf("2: %d + %d\n", a, b);
    return (a-b);
}
f3(a, b) {
    printf("3: %d - %d\n", a, b);
    return (b+a+b);
}

__operator__(*,  f1);
__operator__(>>, f2);
__operator__(|,  f3);

main() {
    printf("result = %d\n", ((2 * 3) >> 4) | 5);
}
