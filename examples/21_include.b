#include "../std/test.b"
#include "foo/add.b"
#include "foo/a.b"
#include "21_foo.b"

main() {
    extrn printf;
    printf("%d, %d, %d\n", add(a(), b()), c, foo);
    assert_equal(39, 13 + 26);
}
