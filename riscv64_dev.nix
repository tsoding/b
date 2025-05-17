{
  pkgs ? import <nixpkgs> {
    localSystem = "x86_64-linux";
    crossSystem = "riscv64-linux-gnu";

  },
}:
pkgs.callPackage (
  {
    mkShell,
  }:
  mkShell {
    # By default this provides gcc, ar, ld, and some other bare minimum tools
  }
) { }
