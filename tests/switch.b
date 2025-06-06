test(a, b) {
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
  extrn printf;
  printf("(69,69)    => %d\n", test(69,69)    );
  printf("(420,420)  => %d\n", test(420,420)  );
  printf("(420,1337) => %d\n", test(420,1337) );
  printf("(420,69)   => %d\n", test(420,69)   );
  printf("(34,35)    => %d\n", test(34,35)    );

  /* According to kbman the syntax of switch-case is `switch rvalue statement` */
  switch 69 {
    unreachable("curly");
  }
  switch 69 unreachable("inline");
}
