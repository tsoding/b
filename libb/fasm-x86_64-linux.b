char  __asm__("xor rax, rax", "mov al, [rdi + rsi]", "ret");
lchar __asm__("mov [rdi + rsi], dl", "ret");
