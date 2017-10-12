{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./default.nix {
  backlight = nixpkgs.xorg.xbacklight;
}
