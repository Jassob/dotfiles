{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./default.nix {
  backlight = nixpkgs.xorg.xbacklight;
}
