#include <stddef.h>

char temp[1024];
size_t temp_bump = 0;

void *temp_alloc(size_t size)
{
    if (temp_bump + size > sizeof(temp)) return NULL;
    void *result = temp + temp_bump;
    temp_bump += size;
    return result;
}

// NOTE: This is fine as "free" because glob_utf8() just frees everything at the end.
void *temp_reset(void)
{
    temp_bump = 0;
}

#define GLOB_IMPLEMENTATION
#define GLOB_MALLOC temp_alloc
#define GLOB_FREE(...) temp_reset()
#include "glob.h"
