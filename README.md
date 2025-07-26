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

## Quick Start

```console
$ make
$ ./build/b -run ./examples/hello_world.b
```

Also check out more examples at [./examples/](./examples/).
Find the project documentation at [./docs/](./docs/).

## Dependencies

Generally, to write programs for the three major contemporary platforms (Linux, Windows, Darwin) you need only these things:

- [Rust](https://www.rust-lang.org/) - the compiler is written in it;
- [GCC](https://gcc.gnu.org/) or [Clang](https://clang.llvm.org/) or [mingw-w64](https://www.mingw-w64.org/) (whatever serves as the `cc` on your POSIX platform) - the `x86_64` and `aarch64` targets generate assembly and pass it to `cc` to assemble and link.

If you feel like playing with our "spicy" targets, you will need to setup few additional things.

### uxn

[Uxn](https://100r.co/site/uxn.html) is a pretty cool small virtual machine designed to be simple and portable.

This compiler toolchain expects the `uxnemu` and `uxncli` executables to be available in the `$PATH` environment variable. We recommend to build them from the source code available at [https://git.sr.ht/~rabbits/uxn](https://git.sr.ht/~rabbits/uxn). Follow their build instructions and then copy the contents of the `bin/` folder somewhere were the `$PATH` points at.

### 6502-posix

[MOS Technology 6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) is a legendary processor that was used in such systems as the Atari 2600, Atari 8-bit computers, Apple II, Nintendo Entertainment System, Commodore 64, Atari Lynx, BBC Micro and others.

Since "targeting 6502" doesn't really mean anything, given the diverse array of hardware, we created a special target called [posix6502](https://github.com/bext-lang/posix6502). It's a simple 6502 emulator that exposes some POSIX functionality to programs running on the 6502 processor, allowing them to operate in a POSIX environment. We use it as a testing and playground target for 6502 codegen. In the future, we may add more 6502-related targets.

It consist of a single `posix6502` executable which is expected to be available to the compiler toolchain via the `$PATH` environment variable. We recommend to build it from the source code:

```console
$ git clone https://github.com/bext-lang/posix6502 && cd posix6502
$ cc -o nob nob.c
$ ./nob
```

Copy the `build/posix6502` executable somewhere were the `$PATH` points at.

### ilasm-mono

This target tries to produce [.NET](https://dotnet.microsoft.com/en-us/)/[Mono](https://www.mono-project.com/) compatible binaries. It's currently a work-in-progress and does not have anything useful implemented in it.

The compiler toolchain expects `ilasm` and `mono` executables available in the `$PATH` environment variables. Lots of Linux distros make them available via the mono packages in their official repos:

```consols
$ sudo xbps-install mono      # Void Linux
$ sudo apt install mono-devel # Ubuntu
...
```

## Contribution

Accepting Pull Requests is currently paused. We are in the middle of [Decentralizing](https://github.com/tsoding/b/issues/62) this repo. The plan is

1. ~~Create a separate organization for the language.~~
2. Keep `*x86_64*` and `*aarch64*` codegens in the main repo.
3. Move codegens [6502](./src/codegen/mos6502.rs) (owner [@Miezekatze64](https://github.com/miezekatze64)), [uxn](./src/codegen/uxn.rs) (owner [@deniska](https://github.com/deniska)), [gas-sh4dsp-prizm](https://github.com/tsoding/b/pull/175) (owner [@seija-amanojaku](https://github.com/seija-amanojaku)) to separate repos within the organization and give the owners full admin access to them.

You can still submit PRs in the meantime. Just don't expect them to be reviewed any time soon since decoupling codegens requires extensive refactoring. The PRs will be addressed eventually.

## References

- https://en.wikipedia.org/wiki/B_(programming_language)
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/bref.html
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/btut.html
