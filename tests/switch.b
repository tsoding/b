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

test_lookup(a, b) {
  switch a {
    case 69:
      return (690);

    case 420:
      switch b {
        case 420:
          return (42);
        case 1337:
          return (7331);
      }
      // default:
      return (-2);

  }
  // default:
  return (-1);
}

/* TODO: maybe this should be a part of the libb at some point?
 * iirc snake example also uses a similar function
 */
unreachable(message) {
  extrn printf, abort;
  printf("UNREACHABLE: %s\n", message);
  abort();
}

test_fallthrough(x) {
    extrn printf;
    switch (x) {
    unreachable("test_fallthrough");
    case 0: printf("0\n");
    case 1: printf("1\n");
    case 2: printf("2\n");
    case 3: printf("3\n");
    case 4: printf("4\n");
    }
}

main() {
  extrn printf, assert_equal;

  assert_equal(test_lookup(69,69),    690,  "(69,69)    => 690");
  assert_equal(test_lookup(420,420),  42,   "(420,420)  => 42");
  assert_equal(test_lookup(420,1337), 7331, "(420,1337) => 7331");
  assert_equal(test_lookup(420,69),   -2,   "(420,69)   => -2");
  assert_equal(test_lookup(34,35),    -1,   "(34,35)    => -1");

  printf("------------------------------\n");
  test_fallthrough(0);
  printf("------------------------------\n");
  test_fallthrough(3);

  /* According to kbman the syntax of switch-case is `switch rvalue statement`.
   * So bellow are valid cases.
   */
  switch 69 {
    unreachable("curly");
  }
  switch 69 unreachable("inline");
}
