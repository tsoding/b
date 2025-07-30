#include <time.h>
#include <stdint.h>

#ifdef _WIN32
#    define WIN32_LEAN_AND_MEAN
#    include <windows.h>
#endif // _WIN32

#define NANOS_PER_SEC 1000000000

// The maximum time span representable is 584 years.
uint64_t nanos_since_unspecified_epoch() {
#ifdef _WIN32
    LARGE_INTEGER Time;
    QueryPerformanceCounter(&Time);

    static LARGE_INTEGER Frequency = {0};
    if (Frequency.QuadPart == 0) {
        QueryPerformanceFrequency(&Frequency);
    }

    uint64_t Secs  = Time.QuadPart / Frequency.QuadPart;
    uint64_t Nanos = Time.QuadPart % Frequency.QuadPart * NANOS_PER_SEC / Frequency.QuadPart;
    return NANOS_PER_SEC * Secs + Nanos;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);

    return NANOS_PER_SEC * ts.tv_sec + ts.tv_nsec;
#endif // _WIN32
}
// TODO: Consider making this a part of https://github.com/tsoding/nob.h
