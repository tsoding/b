char(string, i)     __asm__("xor rax, rax", "mov al, [rcx + rdx]",  "mov rsp, rbp", "pop rbp", "ret");
lchar(string, i, c) __asm__("mov [rcx + rdx], r8l");
