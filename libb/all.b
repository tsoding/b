/* This file contains definitions that are cross-platform and shared across all targets.
 * This usually means that the code in this file is implemented on top of platform specific
 * parts code.
 */

/* This function is primarily used for tests */
assert_equal(actual, expected, message) {
    extrn printf, abort;
    printf("%s: ", message);
    if (actual != expected) {
        printf("FAIL\n");
        abort();
    } else {
        printf("OK\n");
    }
}
