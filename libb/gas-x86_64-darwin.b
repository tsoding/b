sx64 __asm__("movslq %edi, %rax", "ret");
char  __asm__("xorq %rax, %rax", "movb (%rdi, %rsi), %al", "ret");
lchar __asm__("movb %dl, (%rdi, %rsi)", "ret");
extrn printf;
extrn putchar;
extrn getchar;
extrn exit;
