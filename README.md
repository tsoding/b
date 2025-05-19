# B Programming Language

> [!WARNING]
> Compiler is not fully implemented yet.

<p align=center>
  <img src="./logo/logo.png" width=400>
</p>

Compiler for the B Programming Language implemented in [Crust](https://github.com/tsoding/crust)

## Dependencies

- [Rust](https://www.rust-lang.org/) - the compiler is written in it;
- [fasm](https://flatassembler.net/) - used as the compiler backend;
- [clang](https://clang.llvm.org/) - for linking with the C runtime;

## Quick Start

```console
$ make
$ ./build/b -run ./examples/01_hello_world.b
```

Also check out more examples at [./examples/](./examples/).

## References

- https://en.wikipedia.org/wiki/B_(programming_language)
- https://web.archive.org/web/20241214022534/https://www.bell-labs.com/usr/dmr/www/kbman.html
- https://github.com/tsoding/good_training_language
- https://www.felixcloutier.com/x86/
- https://www.scs.stanford.edu/~zyedidia/arm64/
