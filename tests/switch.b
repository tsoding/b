test(a, b) {
  switch a {
    case 35:
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

main() {
  extrn printf;
  printf("(35,69)    => %d\n", test(35,69)    );
  printf("(69,69)    => %d\n", test(69,69)    );
  printf("(420,420)  => %d\n", test(420,420)  );
  printf("(420,1337) => %d\n", test(420,1337) );
  printf("(420,69)   => %d\n", test(420,69)   );
  printf("(34,35)    => %d\n", test(34,35)    );
}
