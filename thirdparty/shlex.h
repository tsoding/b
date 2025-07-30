#ifndef SHLEX_H_
#define SHLEX_H_

#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

// # The Shlex
//
// Both a Lexer and a String Builder which is somewhat POSIX Shell syntax aware.
//
// ## Splitting
//
// ```c
// Shlex s = {0};
// shlex_init(&s, source, source_len);
// while (shlex_next(&s)) {
//     printf("%s\n", s.string);
// }
// shlex_free(&s);
// ```
//
// ## Joining
//
// ```c
// Shlex s = {0};
// shlex_append_quoted(&s, "foo");
// shlex_append_quoted(&s, "bar");
// shlex_append_quoted(&s, "baz");
// shlex_append_quoted(&s, "Hello, World");
// printf("%s\n", shlex_join(&s));
// shlex_free(&s);
// ```
typedef struct {
    // The source which shlex_next(..) is splitting the tokens from.
    const char *source;
    const char *source_end;
    const char *point;

    // An automatically growing string storage which is used for returning tokens by shlex_next(..)
    // and collecting joined strings by shlex_append_quoted[_sized](..).
    char *string;
    size_t string_count;
    size_t string_capacity;
} Shlex;

// Sets the source. It also implies shlex_reset(..), so you don't have to call it explicitly.
void shlex_init(Shlex *s, const char *source, const char *source_end);

// Resets the string storage then chops off a token from the source and appends it to the storage
// with NULL-terminator so you can access it via s.string as a C string.
//
// It also returns s.string as the result on success. Return NULL means ran out of tokens.
char *shlex_next(Shlex *s);

// Resets the state of the shlex removing the source but without deallocating memory of the string
// storage so it can be reused. You usually wanna use this function before calling
// shlex_append_quoted[_sized](..) after doing some splitting with shlex_init(..) and shlex_init(..).
void shlex_reset(Shlex *s);

// Appends a C string to the string storage quoting all the shell unsafe character.
void shlex_append_quoted(Shlex *s, const char *cstr);

// Same as shlex_append_quoted(..) but for sized strings instead of NULL-terminated ones.
void shlex_append_quoted_sized(Shlex *s, const char *str, size_t n);

// Appends a NULL-terminator to the string storage, resets the size of the storage and returns the pointer
// to the C string in the string storage. Usually called after a bunch of calls to
// shlex_append_quoted[_sized](..) to finalize and acquire the result of appending.
// Do not keep the result for too long as it will be very quickly invalidated by pretty much any call to shlex_*(..).
// strdup(..) it to prolong its lifetime.
char *shlex_join(Shlex *s);

// Deallocates the memory of the string storage and completely zeroes out the shlex.
// Generally you don't need to free the shlex like that if you plan to reuse it several times.
// Just do shlex_init(..) or shlex_reset(..) for each input you need to process and it
// is going to reuse the memory it allocated for the string storage every time.
void shlex_free(Shlex *s);

#endif // SHLEX_H_

#ifdef SHLEX_IMPLEMENTATION

static void shlex__string_append(Shlex *s, char x);
static bool shlex__contains_unsafe(const char *str, size_t n);

void shlex_init(Shlex *s, const char *source, const char *source_end)
{
    s->source = source;
    s->source_end = source_end;
    s->point = source;
}

void shlex_free(Shlex *s)
{
    free(s->string);
    memset(s, 0, sizeof(*s));
}

char *shlex_next(Shlex *s)
{
    while (s->point < s->source_end && isspace(*s->point)) {
        s->point++;
    }

    if (s->point >= s->source_end) return NULL;

    s->string_count = 0;
    char strlit = 0;
    while (s->point < s->source_end) {
        switch (strlit) {
        // POSIX.1-2024 - 2.2.2 Single-Quotes
        // > Enclosing characters in single-quotes ('') shall preserve the literal value of each character within the single-quotes. A single-quote cannot occur within single-quotes.
        case '\'':
            if (*s->point == '\'') {
                strlit = 0;
            } else {
                shlex__string_append(s, *s->point);
            }
            s->point++;
            break;
        // POSIX.1-2024 - 2.2.3 Double-Quotes
        // > Enclosing characters in double-quotes ("") shall preserve the literal value of all characters within the double-quotes, with the exception of the characters backquote, <dollar-sign>, and <backslash> ...
        // We only care about <backslash> since <dollar-sign> and backquote is related to semantics of the Shell language. We do a pure lexical analysis.
        case '"':
            switch (*s->point) {
            case '"':
                strlit = 0;
                s->point++;
                break;
            case '\\':
                s->point++;
                // We don't really know what to do with unfinished escape sequences in the context of shlex, so we just interpret the <backslash> literally
                if (s->point >= s->source_end) {
                    shlex__string_append(s, '\\');
                    shlex__string_append(s, '\0');
                    return s->string;
                }

                switch (*s->point) {
                // POSIX.1-2024 - 2.2.3 Double-Quotes
                // > <backslash> shall retain its special meaning as an escape character (see 2.2.1 Escape Character (Backslash)) only when immediately followed by one of the following characters $   `   \   <newline> or by a double-quote character that would otherwise be considered special
                case '$':
                case '`':
                case '\\':
                case '\n':
                case '"':
                    shlex__string_append(s, *s->point);
                    break;
                default:
                    shlex__string_append(s, '\\');
                    shlex__string_append(s, *s->point);
                }
                s->point++;
                break;
            default:
                shlex__string_append(s, *s->point);
                s->point++;
            }
            break;
        case '\0':
            switch (*s->point) {
            case '"':
            case '\'':
                strlit = *s->point;
                s->point++;
                break;
            // POSIX.1-2024 - 2.2.1 Escape Character (Backslash)
            // > A <backslash> that is not quoted shall preserve the literal value of the following character, ...
            case '\\':
                s->point++;
                if (s->point < s->source_end) {
                    shlex__string_append(s, *s->point);
                    s->point++;
                }
                break;
            default:
                if (isspace(*s->point)) {
                    shlex__string_append(s, '\0');
                    return s->string;
                }
                shlex__string_append(s, *s->point);
                s->point++;
            }
            break;
        default:
            assert(0 && "UNREACHABLE");
        }
    }

    shlex__string_append(s, '\0');
    return s->string;
}

void shlex_reset(Shlex *s)
{
    s->source = NULL;
    s->source_end = NULL;
    s->point = NULL;
    s->string_count = 0;
    // Important! Do not touch s->string_capacity and s->string! They are important for reusage of the allocated memory.
}

void shlex_append_quoted(Shlex *s, const char *cstr)
{
    shlex_append_quoted_sized(s, cstr, strlen(cstr));
}

void shlex_append_quoted_sized(Shlex *s, const char *str, size_t n)
{
    if (s->string_count > 0) shlex__string_append(s, ' ');

    if (n == 0) {
        shlex__string_append(s, '\'');
        shlex__string_append(s, '\'');
        return;
    }

    if (!shlex__contains_unsafe(str, n)) {
        for (size_t i = 0; i < n; ++i) {
            shlex__string_append(s, str[i]);
        }
        return;
    }

    shlex__string_append(s, '\'');
    for (size_t i = 0; i < n; ++i) {
        if (str[i] == '\'') {
            shlex__string_append(s, '\'');
            shlex__string_append(s, '"');
            shlex__string_append(s, '\'');
            shlex__string_append(s, '"');
            shlex__string_append(s, '\'');
        } else {
            shlex__string_append(s, str[i]);
        }
    }
    shlex__string_append(s, '\'');
}

char *shlex_join(Shlex *s)
{
    shlex__string_append(s, '\0');
    s->string_count = 0; // Resetting the string storage so it can be reused for more shlex_append_quoted[_sized](..)
    return s->string;
}

static void shlex__string_append(Shlex *s, char x)
{
    if (s->string_count >= s->string_capacity) {
        if (s->string_capacity == 0) {
            s->string_capacity = 256;
        } else {
            s->string_capacity *= 2;
        }
        s->string = realloc(s->string, s->string_capacity);
    }
    s->string[s->string_count++] = x;
}

static bool shlex__contains_unsafe(const char *str, size_t n)
{
    for (size_t i = 0; i < n; ++i) {
        char c = str[i];
        if (!isalnum(c) && strchr("_@%+=:,./-", c) == NULL) {
            return true;
        }
    }
    return false;
}

#endif // SHLEX_IMPLEMENTATION

#ifdef SHLEX_SELF_TEST

#include <stdio.h>

void splitting(void);
void joining(void);
void splitting_joined(void);

int main(void)
{
    splitting();
    joining();
    splitting_joined();
    return 0;
}

void splitting(void)
{
    printf("=== SPLITTING ===\n");
    static const char *sources[] = {
        "Foo Bar",
        "Foo\\ Bar",
        "Foo\\ \\ Bar",
        "-foo -bar -baz",
        "'-foo -bar -baz'",
        "\"Hello, World\"     'Foo Bar'",
        "-I\"./raylib/\" -C link-args=\"-L\\\"./hello world\\\" -lm -lc\" -O3",
    };
    size_t sources_count = sizeof(sources)/sizeof(sources[0]);
    Shlex s = {0};

    for (size_t i = 0; i < sources_count; ++i) {
        if (i > 0) printf("---\n");
        const char *source = sources[i];
        shlex_init(&s, source, source + strlen(source));
        while (shlex_next(&s)) {
            printf("   %s\n", s.string);
        }
    }
    printf("\n");

    shlex_free(&s);
}

void joining(void)
{
    printf("=== JOINING ===\n");
    Shlex s = {0};

    shlex_append_quoted(&s, "foo");
    shlex_append_quoted(&s, "bar");
    shlex_append_quoted(&s, "baz");
    printf("    %s\n", shlex_join(&s));

    shlex_append_quoted(&s, "foo");
    shlex_append_quoted(&s, "bar baz");
    printf("    %s\n", shlex_join(&s));

    shlex_append_quoted(&s, "foo");
    shlex_append_quoted(&s, "bar");
    shlex_append_quoted(&s, "baz");
    shlex_append_quoted(&s, "Hello, 'World'");
    printf("    %s\n", shlex_join(&s));

    shlex_append_quoted(&s, "a'b");
    printf("    %s\n", shlex_join(&s));

    printf("\n");

    shlex_free(&s);
}

void splitting_joined(void)
{

    printf("=== SPLITTING JOINED ===\n");
    Shlex s = {0};
    shlex_append_quoted(&s, "foo");
    shlex_append_quoted(&s, "bar");
    shlex_append_quoted(&s, "baz");
    shlex_append_quoted(&s, "Hello, 'World'");
    char *source = strdup(shlex_join(&s));

    shlex_init(&s, source, source + strlen(source));
    while (shlex_next(&s)) {
        printf("    %s\n", s.string);
    }

    shlex_free(&s);
    free(source);
}

#endif // SHLEX_SELF_TEST
