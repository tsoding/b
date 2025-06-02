#include <stdio.h>

// Linux defines `stdin`, `stdout` and `stderr` as both an external symbol and a macro.
// however Windows defines them as macros only, and these macros expand to non-const
// internal msvcrt functions, so they can't be linked with rust through standard ffi.
// this is a workaround to be able to access `stdio` constant from rust, it's also
// be used on linux as well to keep the interface consistant 

FILE *get_stdin() { return stdin; }
FILE *get_stdout() { return stdout; }
FILE *get_stderr() { return stderr; }
