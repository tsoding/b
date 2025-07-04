__variadic__(printf, 1);

char __asm__(
    "ldrb w0, [x0, x1]",
    "ret"
);

lchar __asm__(
    "strb w2, [x0, x1]",
    "ret"
);
