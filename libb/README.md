# libb - The Standard Library of B

Here we describe the bare minium of the functionality that libb must provide. It may provide more on some platforms if necessary.

Some platforms like `gas-x86_64-linux`, `gas-aarch64-linux`, etc also link with libc, which means some of the functionality of libb is covered by libc. For platforms that do not link with libc (like `uxn`, `6502`, etc) the required functionality should be implemented from scratch.

If you don't want to link with libb (and libc on the platforms where it's available) use the flag `-nostdlib`.

## Expected functions and globals

Loosely based on `8.0 Library Functions` from [kbman][kbman]. May contain additional historically inaccurate things.

<!-- TODO: document the main(argc, argv) functionality that is provided by libb -->

| Signature                    | Description                                                                                                      |
|------------------------------|------------------------------------------------------------------------------------------------------------------|
| `c = char(string, i);`       | The i-th character of the string is returned;                                                                    |
| `lchar(string, i, char);`    | The character char is stored in the i-th character of the string.                                                |
| `exit(code);`                | The current process is terminated;                                                                               |
| `char = getchar();`          | The next character form the standard input file is returned. The character `\\e` is returned for an end-of-file. |
| `putchar(char);`             | The character char is written on the standard output file.                                                       |
| `printf(format, argl, ...);` | See `9.3` of [kbman][kbman]. libc may implement more things, but `9.3` is the minimum.                           |
| `printn(number, base);`      | See `9.1` of [kbman][kbman].                                                                                     |
| ...                          | ...                                                                                                              |

[kbman]: (https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html)
