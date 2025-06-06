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

unreachable(message) {
  extrn printf, abort;
  printf("UNREACHABLE: %s\n", message);
  abort();
}

main() {
  extrn printf, assert_equal;

  assert_equal(test_lookup(69,69),    690,  "(69,69)    => 690");
  assert_equal(test_lookup(420,420),  42,   "(420,420)  => 42");
  assert_equal(test_lookup(420,1337), 7331, "(420,1337) => 7331");
  assert_equal(test_lookup(420,69),   -2,   "(420,69)   => -2");
  assert_equal(test_lookup(34,35),    -1,   "(34,35)    => -1");

  /* According to kbman the syntax of switch-case is `switch rvalue statement`.
   * So bellow are valid cases.
   */
  switch 69 {
    unreachable("curly");
  }
  switch 69 unreachable("inline");
}
