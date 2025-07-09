#include "nob.h"
#define GLOB_IMPLEMENTATION
#define GLOB_MALLOC nob_temp_alloc
#define GLOB_FREE(...)
#include "glob.h"
