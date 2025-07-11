sx64 __asm__("sxtw x0, w0", "ret");
char __asm__("ldrb w0, [x0, x1]", "ret");
lchar __asm__("strb w2, [x0, x1]", "ret");
extrn printf;
extrn putchar;
extrn getchar;
extrn exit;
