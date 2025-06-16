char(string, i)     __asm__("xor rax, rax", "mov al, [rdi + rsi]", "mov rsp, rbp", "pop rbp", "ret");
lchar(string, i, c) __asm__("mov [rdi + rsi], dl");
