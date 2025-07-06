sx64 __asm__("movsxd rax, ecx", "ret");
char  __asm__("xor rax, rax", "mov al, [rcx + rdx]",  "ret");
lchar __asm__("mov [rcx + rdx], r8l", "ret");
