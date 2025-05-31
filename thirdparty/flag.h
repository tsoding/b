// flag.h -- v1.2.0 -- command-line flag parsing
//
//   Inspired by Go's flag module: https://pkg.go.dev/flag
//
// Macros API:
// - FLAG_LIST_INIT_CAP - initial capacity of the Flag_List Dynamic Array.
// - FLAGS_CAP - how many flags you can define.
// - FLAG_PUSH_DASH_DASH_BACK - make flag_parse() retain "--" in the rest args
//   (available via flag_rest_argc() and flag_rest_argv()). Useful when you need
//   to know whether flag_parse() has stopped due to encountering "--" or due to
//   encountering a non-flag. Ideally this should've been a default behavior,
//   but it breaks backward compatibility. Hence it's a feature macro.
//   TODO: make FLAG_PUSH_DASH_DASH_BACK a default behavior on a major version upgrade.
#ifndef FLAG_H_
#define FLAG_H_

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <inttypes.h>
#include <limits.h>
#include <string.h>
#include <errno.h>

// TODO: add support for -flag=x syntax
// TODO: *_var function variants
// void flag_bool_var(bool *var, const char *name, bool def, const char *desc);
// void flag_bool_uint64(uint64_t *var, const char *name, bool def, const char *desc);
// etc.
// WARNING! *_var functions may break the flag_name() functionality

#ifndef FLAG_LIST_INIT_CAP
#define FLAG_LIST_INIT_CAP 8
#endif // FLAG_LIST_INIT_CAP

typedef struct {
    const char **items;
    size_t count;
    size_t capacity;
} Flag_List;

char *flag_name(void *val);
bool *flag_bool(const char *name, bool def, const char *desc);
uint64_t *flag_uint64(const char *name, uint64_t def, const char *desc);
size_t *flag_size(const char *name, uint64_t def, const char *desc);
char **flag_str(const char *name, const char *def, const char *desc);
Flag_List *flag_list(const char *name, const char *desc);
bool flag_parse(int argc, char **argv);
int flag_rest_argc(void);
char **flag_rest_argv(void);
void flag_print_error(FILE *stream);
void flag_print_options(FILE *stream);

#endif // FLAG_H_

//////////////////////////////

#ifdef FLAG_IMPLEMENTATION

typedef enum {
    FLAG_BOOL = 0,
    FLAG_UINT64,
    FLAG_SIZE,
    FLAG_STR,
    FLAG_LIST,
    COUNT_FLAG_TYPES,
} Flag_Type;

static_assert(COUNT_FLAG_TYPES == 5, "Exhaustive Flag_Value definition");
typedef union {
    char *as_str;
    uint64_t as_uint64;
    bool as_bool;
    size_t as_size;
    Flag_List as_list;
} Flag_Value;

typedef enum {
    FLAG_NO_ERROR = 0,
    FLAG_ERROR_UNKNOWN,
    FLAG_ERROR_NO_VALUE,
    FLAG_ERROR_INVALID_NUMBER,
    FLAG_ERROR_INTEGER_OVERFLOW,
    FLAG_ERROR_INVALID_SIZE_SUFFIX,
    COUNT_FLAG_ERRORS,
} Flag_Error;

typedef struct {
    Flag_Type type;
    char *name;
    char *desc;
    Flag_Value val;
    Flag_Value def;
} Flag;

#ifndef FLAGS_CAP
#define FLAGS_CAP 256
#endif

typedef struct {
    Flag flags[FLAGS_CAP];
    size_t flags_count;

    Flag_Error flag_error;
    char *flag_error_name;

    const char *program_name;

    int rest_argc;
    char **rest_argv;
} Flag_Context;

static Flag_Context flag_global_context;

Flag *flag_new(Flag_Type type, const char *name, const char *desc)
{
    Flag_Context *c = &flag_global_context;

    assert(c->flags_count < FLAGS_CAP);
    Flag *flag = &c->flags[c->flags_count++];
    memset(flag, 0, sizeof(*flag));
    flag->type = type;
    // NOTE: I won't touch them I promise Kappa
    flag->name = (char*) name;
    flag->desc = (char*) desc;
    return flag;
}

char *flag_name(void *val)
{
    Flag *flag = (Flag*) ((char*) val - offsetof(Flag, val));
    return flag->name;
}

bool *flag_bool(const char *name, bool def, const char *desc)
{
    Flag *flag = flag_new(FLAG_BOOL, name, desc);
    flag->def.as_bool = def;
    flag->val.as_bool = def;
    return &flag->val.as_bool;
}

uint64_t *flag_uint64(const char *name, uint64_t def, const char *desc)
{
    Flag *flag = flag_new(FLAG_UINT64, name, desc);
    flag->val.as_uint64 = def;
    flag->def.as_uint64 = def;
    return &flag->val.as_uint64;
}

size_t *flag_size(const char *name, uint64_t def, const char *desc)
{
    Flag *flag = flag_new(FLAG_SIZE, name, desc);
    flag->val.as_size = def;
    flag->def.as_size = def;
    return &flag->val.as_size;
}

char **flag_str(const char *name, const char *def, const char *desc)
{
    Flag *flag = flag_new(FLAG_STR, name, desc);
    flag->val.as_str = (char*) def;
    flag->def.as_str = (char*) def;
    return &flag->val.as_str;
}

Flag_List *flag_list(const char *name, const char *desc)
{
    Flag *flag = flag_new(FLAG_LIST, name, desc);
    return &flag->val.as_list;
}

static void flag_list_append(Flag_List *list, char *item)
{
    if (list->count >= list->capacity) {
        size_t new_capacity = list->capacity == 0 ? FLAG_LIST_INIT_CAP : list->capacity*2;
        list->items = (const char**)realloc(list->items, new_capacity*sizeof(*list->items));
        list->capacity = new_capacity;
    }

    list->items[list->count++] = item;
}

static char *flag_shift_args(int *argc, char ***argv)
{
    assert(*argc > 0);
    char *result = **argv;
    *argv += 1;
    *argc -= 1;
    return result;
}

int flag_rest_argc(void)
{
    return flag_global_context.rest_argc;
}

char **flag_rest_argv(void)
{
    return flag_global_context.rest_argv;
}

const char *flag_program_name(void)
{
    return flag_global_context.program_name;
}

bool flag_parse(int argc, char **argv)
{
    Flag_Context *c = &flag_global_context;

    if (c->program_name == NULL) {
        c->program_name = flag_shift_args(&argc, &argv);
    }

    while (argc > 0) {
        char *flag = flag_shift_args(&argc, &argv);

        if (*flag != '-') {
            // NOTE: pushing flag back into args
            c->rest_argc = argc + 1;
            c->rest_argv = argv - 1;
            return true;
        }

        if (strcmp(flag, "--") == 0) {
#ifdef FLAG_PUSH_DASH_DASH_BACK
            // NOTE: pushing dash dash back into args as requested by the user
            c->rest_argc = argc + 1;
            c->rest_argv = argv - 1;
#else
            // NOTE: not pushing dash dash back into args for backward compatibility
            c->rest_argc = argc;
            c->rest_argv = argv;
#endif // FLAG_PUSH_DASH_DASH_BACK
            return true;
        }

        // NOTE: remove the dash
        flag += 1;

        bool found = false;
        for (size_t i = 0; i < c->flags_count; ++i) {
            if (strcmp(c->flags[i].name, flag) == 0) {
                static_assert(COUNT_FLAG_TYPES == 5, "Exhaustive flag type parsing");
                switch (c->flags[i].type) {
                case FLAG_LIST: {
                    if (argc == 0) {
                        c->flag_error = FLAG_ERROR_NO_VALUE;
                        c->flag_error_name = flag;
                        return false;
                    }

                    char *arg = flag_shift_args(&argc, &argv);
                    flag_list_append(&c->flags[i].val.as_list, arg);
                }
                break;

                case FLAG_BOOL: {
                    c->flags[i].val.as_bool = true;
                }
                break;

                case FLAG_STR: {
                    if (argc == 0) {
                        c->flag_error = FLAG_ERROR_NO_VALUE;
                        c->flag_error_name = flag;
                        return false;
                    }
                    char *arg = flag_shift_args(&argc, &argv);
                    c->flags[i].val.as_str = arg;
                }
                break;

                case FLAG_UINT64: {
                    if (argc == 0) {
                        c->flag_error = FLAG_ERROR_NO_VALUE;
                        c->flag_error_name = flag;
                        return false;
                    }
                    char *arg = flag_shift_args(&argc, &argv);

                    static_assert(sizeof(unsigned long long int) == sizeof(uint64_t), "The original author designed this for x86_64 machine with the compiler that expects unsigned long long int and uint64_t to be the same thing, so they could use strtoull() function to parse it. Please adjust this code for your case and maybe even send the patch to upstream to make it work on a wider range of environments.");
                    char *endptr;
                    // TODO: replace strtoull with a custom solution
                    // That way we can get rid of the dependency on errno and static_assert
                    unsigned long long int result = strtoull(arg, &endptr, 10);

                    if (*endptr != '\0') {
                        c->flag_error = FLAG_ERROR_INVALID_NUMBER;
                        c->flag_error_name = flag;
                        return false;
                    }

                    if (result == ULLONG_MAX && errno == ERANGE) {
                        c->flag_error = FLAG_ERROR_INTEGER_OVERFLOW;
                        c->flag_error_name = flag;
                        return false;
                    }

                    c->flags[i].val.as_uint64 = result;
                }
                break;

                case FLAG_SIZE: {
                    if (argc == 0) {
                        c->flag_error = FLAG_ERROR_NO_VALUE;
                        c->flag_error_name = flag;
                        return false;
                    }
                    char *arg = flag_shift_args(&argc, &argv);

                    static_assert(sizeof(unsigned long long int) == sizeof(size_t), "The original author designed this for x86_64 machine with the compiler that expects unsigned long long int and size_t to be the same thing, so they could use strtoull() function to parse it. Please adjust this code for your case and maybe even send the patch to upstream to make it work on a wider range of environments.");
                    char *endptr;
                    // TODO: replace strtoull with a custom solution
                    // That way we can get rid of the dependency on errno and static_assert
                    unsigned long long int result = strtoull(arg, &endptr, 10);

                    // TODO: handle more multiplicative suffixes like in dd(1). From the dd(1) man page:
                    // > N and BYTES may be followed by the following
                    // > multiplicative suffixes: c =1, w =2, b =512, kB =1000, K
                    // > =1024, MB =1000*1000, M =1024*1024, xM =M, GB
                    // > =1000*1000*1000, G =1024*1024*1024, and so on for T, P,
                    // > E, Z, Y.
                    if (strcmp(endptr, "K") == 0) {
                        result *= 1024;
                    } else if (strcmp(endptr, "M") == 0) {
                        result *= 1024*1024;
                    } else if (strcmp(endptr, "G") == 0) {
                        result *= 1024*1024*1024;
                    } else if (strcmp(endptr, "") != 0) {
                        c->flag_error = FLAG_ERROR_INVALID_SIZE_SUFFIX;
                        c->flag_error_name = flag;
                        // TODO: capability to report what exactly is the wrong suffix
                        return false;
                    }

                    if (result == ULLONG_MAX && errno == ERANGE) {
                        c->flag_error = FLAG_ERROR_INTEGER_OVERFLOW;
                        c->flag_error_name = flag;
                        return false;
                    }

                    c->flags[i].val.as_size = result;
                }
                break;

                case COUNT_FLAG_TYPES:
                default: {
                    assert(0 && "unreachable");
                    exit(69);
                }
                }

                found = true;
            }
        }

        if (!found) {
            c->flag_error = FLAG_ERROR_UNKNOWN;
            c->flag_error_name = flag;
            return false;
        }
    }

    c->rest_argc = argc;
    c->rest_argv = argv;
    return true;
}

void flag_print_options(FILE *stream)
{
    Flag_Context *c = &flag_global_context;
    for (size_t i = 0; i < c->flags_count; ++i) {
        Flag *flag = &c->flags[i];

        fprintf(stream, "    -%s\n", flag->name);
        fprintf(stream, "        %s\n", flag->desc);
        static_assert(COUNT_FLAG_TYPES == 5, "Exhaustive flag type defaults printing");
        switch (c->flags[i].type) {
        case FLAG_LIST:
            break;
        case FLAG_BOOL:
            if (flag->def.as_bool) {
                fprintf(stream, "        Default: %s\n", flag->def.as_bool ? "true" : "false");
            }
            break;
        case FLAG_UINT64:
            fprintf(stream, "        Default: %" PRIu64 "\n", flag->def.as_uint64);
            break;
        case FLAG_SIZE:
            fprintf(stream, "        Default: %zu\n", flag->def.as_size);
            break;
        case FLAG_STR:
            if (flag->def.as_str) {
                fprintf(stream, "        Default: %s\n", flag->def.as_str);
            }
            break;
        default:
            assert(0 && "unreachable");
            exit(69);
        }
    }
}

void flag_print_error(FILE *stream)
{
    Flag_Context *c = &flag_global_context;
    static_assert(COUNT_FLAG_ERRORS == 6, "Exhaustive flag error printing");
    switch (c->flag_error) {
    case FLAG_NO_ERROR:
        // NOTE: don't call flag_print_error() if flag_parse() didn't return false, okay? ._.
        fprintf(stream, "Operation Failed Successfully! Please tell the developer of this software that they don't know what they are doing! :)");
        break;
    case FLAG_ERROR_UNKNOWN:
        fprintf(stream, "ERROR: -%s: unknown flag\n", c->flag_error_name);
        break;
    case FLAG_ERROR_NO_VALUE:
        fprintf(stream, "ERROR: -%s: no value provided\n", c->flag_error_name);
        break;
    case FLAG_ERROR_INVALID_NUMBER:
        fprintf(stream, "ERROR: -%s: invalid number\n", c->flag_error_name);
        break;
    case FLAG_ERROR_INTEGER_OVERFLOW:
        fprintf(stream, "ERROR: -%s: integer overflow\n", c->flag_error_name);
        break;
    case FLAG_ERROR_INVALID_SIZE_SUFFIX:
        fprintf(stream, "ERROR: -%s: invalid size suffix\n", c->flag_error_name);
        break;
    case COUNT_FLAG_ERRORS:
    default:
        assert(0 && "unreachable");
        exit(69);
    }
}

#endif // FLAG_IMPLEMENTATION

/*
   Revision history:

     1.2.0 (2025-05-31) Introduce FLAG_PUSH_DASH_DASH_BACK (by @nullnominal)
     1.1.0 (2025-05-09) Introduce flag list
     1.0.0 (2025-03-03) Initial release
                        Save program_name in the context

*/

// Copyright 2021 Alexey Kutepov <reximkut@gmail.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
