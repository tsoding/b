sx64 __asm__("xorq %rax, %rax", "movslq %ecx, %rax", "ret");
char  __asm__("xorq %rax, %rax", "movb (%rcx, %rdx), %al", "ret");
lchar __asm__("movb %r8b, (%rcx, %rdx)", "ret");
extrn printf;
extrn putchar;
extrn getchar;
extrn exit;
