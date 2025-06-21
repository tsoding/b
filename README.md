# B Programming Language

> [!WARNING]
> Compiler is not fully implemented yet.

<p align=center>
  <img src="./logo/logo_strawberry.png" width=400>
</p>

<p align=center>
  <sub>Logo by Strawberry 🍓</sub>
</p>

Compiler for the B Programming Language implemented in [Crust](https://github.com/tsoding/crust)

## Dependencies

- [Rust](https://www.rust-lang.org/) - the compiler is written in it;
- [fasm](https://flatassembler.net/) - used as the compiler backend;

### Target-Specific Dependencies

- **fasm-x86_64-windows**: When cross-compiling from Linux to Windows, you may need [Wine](https://www.winehq.org/) installed as a system package to run the compiled Windows executables with the `-run` flag.

## Quick Start

```console
$ make
$ ./build/b -run ./examples/hello_world.b
```

Also check out more examples at [./examples/](./examples/).

## Testing

The project comes with [btest](./src/btest.rs) utility which tests the B compiler. It is built automatically along with the B compiler when you do `make`.

When you run it, it just builds and runs all the tests from the [./tests/](./tests/) folder on all the supported targets and generates a matrix report. Give it a try to see it for yourself:

```
$ make
$ ./build/btest
```

It doesn't crash when it encounters errors, it just collects the statuses of the tests on all the platforms to give an overview of the current state of the compiler.

### Slicing the Test Matrix

If you want to test only on a specific platform you can supply the flag `-t`

```console
$ ./build/btest -t fasm-x86_64-linux
```

You can supply several platforms

```console
$ ./build/btest -t fasm-x86_64-linux -t uxn
```

If you want to run a specific test case you can supply flag `-c`

```console
$ ./build/btest -c upper
```

You can do several tests

```console
$ ./build/btest -c upper -c vector
```

And of course you can combine both `-c` and `-t` flags to slice the Test Matrix however you want

```console
$ ./build/btest -c upper -c vector -t fasm-x86_64-linux -t uxn
```

## References

- https://en.wikipedia.org/wiki/B_(programming_language)
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html
- https://github.com/tsoding/good_training_language
- https://www.felixcloutier.com/x86/
- https://www.scs.stanford.edu/~zyedidia/arm64/
