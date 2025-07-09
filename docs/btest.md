# btest

The project comes with [btest](../src/btest.rs) utility which tests the B compiler. It is built automatically along with the B compiler when you do `make`.

When you run it, it just builds and runs all the tests from the [../tests/](./tests/) folder on all the supported targets and generates a matrix report. Give it a try to see it for yourself:

```
$ make
$ ./build/btest
```

It doesn't crash when it encounters errors, it just collects the statuses of the tests on all the platforms to give an overview of the current state of the compiler.

## Slicing the Test Matrix

If you want to test only on a specific platform you can supply the flag `-t`.

```console
$ ./build/btest -t fasm-x86_64-linux
```

You can supply several platforms.

```console
$ ./build/btest -t fasm-x86_64-linux -t uxn
```

If you want to run a specific test case you can supply flag `-c`.

```console
$ ./build/btest -c upper
```

You can do several tests.

```console
$ ./build/btest -c upper -c vector
```

And of course you can combine both `-c` and `-t` flags to slice the Test Matrix however you want.

```console
$ ./build/btest -c upper -c vector -t fasm-x86_64-linux -t uxn
```

Both flags accept [glob](https://en.wikipedia.org/wiki/Glob_(programming)) patterns.

```console
$ ./build/btest -t *linux -c *linux
```

If you want to exclude a specific platform you can supply the flag `-xt`.

```console
$ ./build/btest -xt fasm-x86_64-linux
```

`-xt` also accepts [glob](https://en.wikipedia.org/wiki/Glob_(programming)) patterns.

```console
$ ./build/btest -xt *linux
```

`-xc` is just like `-xt` but for test cases.

```console
$ ./build/btest -xt *linux -xc asm*
```

<!-- TODO: document -record and tests.json -->
