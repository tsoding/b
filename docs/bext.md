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

<!-- TODO: document the rest of the extensions and deviations -->
