# B Programming Language

> [!WARNING]
> The Compiler is not fully implemented yet. Plus on its own it's probably not useful for any serious Software Development. It is fun for Recreational Programming though.

<p align=center>
  <img src="./logo/logo_strawberry.png" width=400>
</p>

<p align=center>
  <sub>Logo by Strawberry 🍓</sub>
</p>

Compiler for the [B Programming Language](https://en.wikipedia.org/wiki/B_(programming_language)) implemented in [Crust](https://github.com/tsoding/crust).

## Dependencies

- [Rust](https://www.rust-lang.org/) - the compiler is written in it;
- [GCC](https://gcc.gnu.org/) or [Clang](https://clang.llvm.org/) (whatever surves as the `cc` on your POSIX platform) - the `x86_64` and `aarch64` targets generate assembly and pass it to `cc` to assemble and link.

<!-- TODO: document specific dependencies for the rest of the targets. Like mingw32-w64 and wine on Linux for gas-x86_64-Windows, etc. -->

## Quick Start

```console
$ make
$ ./build/b -run ./examples/hello_world.b
```

Also check out more examples at [./examples/](./examples/).
Find the project documentation at [./docs/](./docs/).

## References

- https://en.wikipedia.org/wiki/B_(programming_language)
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/bref.html
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/btut.html
