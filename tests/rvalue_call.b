main() {
    extrn printf, malloc, exit;
    auto funs;

    auto W;
    W = 8;

    funs = malloc(4 * W);
    funs[0] = &printf;
    funs[W] = &exit;

    // parentheses needed, because stacked postfix not yet supported
    (funs[0])("Hello, World!\n");
    (funs[W])(0);
}
