// Jim 2.0
//
// Current version of Jim. Main differences from Jim 1.0 are
// - Using Dynamic Arrays for scopes allowing them to be arbitrarily nested,
// - Collecting the output into a sink which is a String Builder now, delegating all the IO hustle to the user of the library,
// - Lack of Jim_Error mechanism, which dealt with IO errors and invalid usage of the API. Since we don't deal with IO anymore we have no IO errors. And invalid usage of the API is simply assert()-ed.

#ifndef JIM_H_
#define JIM_H_

#ifndef JIM_SCOPES_CAPACITY
#define JIM_SCOPES_CAPACITY 128
#endif // JIM_SCOPES_CAPACITY

#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    JIM_ARRAY_SCOPE,
    JIM_OBJECT_SCOPE,
} Jim_Scope_Kind;

typedef struct {
    Jim_Scope_Kind kind;
    int tail;
    int key;
} Jim_Scope;

typedef struct {
    char *sink;
    size_t sink_count;
    size_t sink_capacity;
    Jim_Scope *scopes;
    size_t scopes_count;
    size_t scopes_capacity;
} Jim;

// TODO: implement pretty-printing based on how nested the scopes are.
//   Introduce a separate boolean flag in the Jim struct to enable/disable
//   the pretty-printing.

void jim_begin(Jim *jim);
void jim_null(Jim *jim);
void jim_bool(Jim *jim, int boolean);
void jim_integer(Jim *jim, long long int x);
// TODO: deprecate this version of jim_float introduce the one that does not require precision and uses something like sprintf from libc to render the floats
void jim_float(Jim *jim, double x, int precision);
void jim_string(Jim *jim, const char *str);
void jim_string_sized(Jim *jim, const char *str, size_t size);

void jim_element_begin(Jim *jim);
void jim_element_end(Jim *jim);

void jim_array_begin(Jim *jim);
void jim_array_end(Jim *jim);

void jim_object_begin(Jim *jim);
void jim_member_key(Jim *jim, const char *str);
void jim_member_key_sized(Jim *jim, const char *str, size_t size);
void jim_object_end(Jim *jim);

#endif // JIM_H_

#ifdef JIM_IMPLEMENTATION

static void jim_scope_push(Jim *jim, Jim_Scope_Kind kind)
{
    if (jim->scopes_count >= jim->scopes_capacity) {
        if (jim->scopes_capacity == 0) jim->scopes_capacity = JIM_SCOPES_CAPACITY;
        else jim->scopes_capacity *= 2;
        jim->scopes = realloc(jim->scopes, sizeof(*jim->scopes)*jim->scopes_capacity);
        assert(jim->scopes);
    }
    jim->scopes[jim->scopes_count].kind = kind;
    jim->scopes[jim->scopes_count].tail = 0;
    jim->scopes[jim->scopes_count].key = 0;
    jim->scopes_count += 1;
}

static void jim_scope_pop(Jim *jim)
{
    assert(jim->scopes_count > 0);
    jim->scopes_count--;
}

static Jim_Scope *jim_current_scope(Jim *jim)
{
    if (jim->scopes_count > 0) {
        return &jim->scopes[jim->scopes_count - 1];
    }
    return NULL;
}

static void jim_write(Jim *jim, const char *buffer, size_t size)
{
    while (jim->sink_count + size >= jim->sink_capacity) {
        // TODO: rename JIM_SCOPES_CAPACITY to something else since it's used by both sink and scopes
        if (jim->sink_capacity == 0) jim->sink_capacity = JIM_SCOPES_CAPACITY;
        else jim->sink_capacity *= 2;
        jim->sink = realloc(jim->sink, sizeof(*jim->sink)*jim->sink_capacity);
    }
    memcpy(jim->sink + jim->sink_count, buffer, size);
    jim->sink_count += size;
}

static void jim_write_cstr(Jim *jim, const char *cstr)
{
    jim_write(jim, cstr, strlen(cstr));
}

static int jim_get_utf8_char_len(unsigned char ch)
{
    if ((ch & 0x80) == 0) return 1;
    switch (ch & 0xf0) {
    case 0xf0:
        return 4;
    case 0xe0:
        return 3;
    default:
        return 2;
    }
}

void jim_begin(Jim *jim)
{
    jim->sink_count = 0;
    jim->scopes_count = 0;
}

void jim_element_begin(Jim *jim)
{
    Jim_Scope *scope = jim_current_scope(jim);
    if (scope && scope->tail && !scope->key) {
        jim_write_cstr(jim, ",");
    }
}

void jim_element_end(Jim *jim)
{
    Jim_Scope *scope = jim_current_scope(jim);
    if (scope) {
        scope->tail = 1;
        scope->key = 0;
    }
}

void jim_null(Jim *jim)
{
    jim_element_begin(jim);
    jim_write_cstr(jim, "null");
    jim_element_end(jim);
}

void jim_bool(Jim *jim, int boolean)
{
    jim_element_begin(jim);
    if (boolean) {
        jim_write_cstr(jim, "true");
    } else {
        jim_write_cstr(jim, "false");
    }
    jim_element_end(jim);
}

static void jim_integer_no_element(Jim *jim, long long int x)
{
    if (x < 0) {
        jim_write_cstr(jim, "-");
        x = -x;
    }

    if (x == 0) {
        jim_write_cstr(jim, "0");
    } else {
        char buffer[64];
        size_t count = 0;

        while (x > 0) {
            buffer[count++] = (x % 10) + '0';
            x /= 10;
        }

        for (size_t i = 0; i < count / 2; ++i) {
            char t = buffer[i];
            buffer[i] = buffer[count - i - 1];
            buffer[count - i - 1] = t;
        }

        jim_write(jim, buffer, count);
    }
}

void jim_integer(Jim *jim, long long int x)
{
    jim_element_begin(jim);
    jim_integer_no_element(jim, x);
    jim_element_end(jim);
}

static int is_nan_or_inf(double x)
{
    unsigned long long int mask = (1ULL << 11ULL) - 1ULL;
    return (((*(unsigned long long int*) &x) >> 52ULL) & mask) == mask;
}

void jim_float(Jim *jim, double x, int precision)
{
    if (is_nan_or_inf(x)) {
        jim_null(jim);
    } else {
        jim_element_begin(jim);

        jim_integer_no_element(jim, (long long int) x);
        x -= (double) (long long int) x;
        while (precision-- > 0) {
            x *= 10.0;
        }
        jim_write_cstr(jim, ".");

        long long int y = (long long int) x;
        if (y < 0) {
            y = -y;
        }
        jim_integer_no_element(jim, y);

        jim_element_end(jim);
    }
}

static void jim_string_sized_no_element(Jim *jim, const char *str, size_t size)
{
    const char *hex_digits = "0123456789abcdef";
    const char *specials = "btnvfr";
    const char *p = str;
    size_t len = size;

    jim_write_cstr(jim, "\"");
    size_t cl;
    for (size_t i = 0; i < len; i++) {
        unsigned char ch = ((unsigned char *) p)[i];
        if (ch == '"' || ch == '\\') {
            jim_write(jim, "\\", 1);
            jim_write(jim, p + i, 1);
        } else if (ch >= '\b' && ch <= '\r') {
            jim_write(jim, "\\", 1);
            jim_write(jim, &specials[ch - '\b'], 1);
        } else if (0x20 <= ch && ch <= 0x7F) { // is printable
        jim_write(jim, p + i, 1);
    } else if ((cl = jim_get_utf8_char_len(ch)) == 1) {
        jim_write(jim, "\\u00", 4);
        jim_write(jim, &hex_digits[(ch >> 4) % 0xf], 1);
        jim_write(jim, &hex_digits[ch % 0xf], 1);
    } else {
        jim_write(jim, p + i, cl);
        i += cl - 1;
    }
}

jim_write_cstr(jim, "\"");
}

void jim_string_sized(Jim *jim, const char *str, size_t size)
{
    jim_element_begin(jim);
    jim_string_sized_no_element(jim, str, size);
    jim_element_end(jim);
}

void jim_string(Jim *jim, const char *str)
{
    jim_string_sized(jim, str, strlen(str));
}

void jim_array_begin(Jim *jim)
{
    jim_element_begin(jim);
    jim_write_cstr(jim, "[");
    jim_scope_push(jim, JIM_ARRAY_SCOPE);
}


void jim_array_end(Jim *jim)
{
    jim_write_cstr(jim, "]");
    jim_scope_pop(jim);
    jim_element_end(jim);
}

void jim_object_begin(Jim *jim)
{
    jim_element_begin(jim);
    jim_write_cstr(jim, "{");
    jim_scope_push(jim, JIM_OBJECT_SCOPE);
}

void jim_member_key(Jim *jim, const char *str)
{
    jim_member_key_sized(jim, str, strlen(str));
}

void jim_member_key_sized(Jim *jim, const char *str, size_t size)
{
    jim_element_begin(jim);
    Jim_Scope *scope = jim_current_scope(jim);
    assert(scope);
    assert(scope->kind == JIM_OBJECT_SCOPE);
    assert(!scope->key);
    jim_string_sized_no_element(jim, str, size);
    jim_write_cstr(jim, ":");
    scope->key = 1;
}

void jim_object_end(Jim *jim)
{
    jim_write_cstr(jim, "}");
    jim_scope_pop(jim);
    jim_element_end(jim);
}

#endif // JIM_IMPLEMENTATION
