# B extensions

Here we document all the things that deviate or extend the original description of the B programming language from [kbman](https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html).

## Top Level `extrn` declarations

```c
main() {
    printf("Hello, World\n");
}

extrn printf;
```

`printf` is now visible to all the functions in the global scope.

## Inline assembly
```c
main() {
    // for gas-x86_64-linux
    __asm__(
    "movq $60, %rax", // exit syscall number
    "movq $69, %rdi", // exit code
    "syscall"
    );
}
```

`__asm__` is a function-like statement that takes a list of string literals as arguments and passes them directly to the assembler.

## Naked functions

```c
// for gas-x86_64-linux
main __asm__(
    "movq $69, %rax",
    "ret"
);
```

These are a special kind of function for which the compiler does not generate a prologue or an epilogue. \
the syntax is `name __asm__(...);` (this is different from `name() __asm__(...);`).

## \_\_variadic\_\_

```c
extrn printf;
__variadic__(printf, 1);

main() {
    printf("Hello, world!\n");
}
```

Some targets like `gas-aarch64-darwin` have a different calling convention for variadic functions from normal ones, \
this is needed to make the compiler use the correct calling convention. \
the syntax is `__variadic__(function_name, number_of_fixed_args);`

<!--
    TODO: hex-literals and C++ style comments are currently considered deviations
    and not extensions, thus disabled in historical mode, which is a bug.
    Add them here once the issue is resolved.

    TODO: using `\` instead of `*` as the string escape character is
    a deviation and not an extension, should it be documented here?
-->

<!-- TODO: document the rest of the extensions and deviations -->
