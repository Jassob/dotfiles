# xmonad

This is my XMonad config, with an attempt of making it work in NixOS.

## Getting started

Clone the repo somewhere and symlink this folder (xmonad/) to
~/.xmonad or ~/.config/xmonad/ and then run the following command
inside that folder.

``` .bash
$ cabal2nix --shell > shell.nix
$ nix-shell shell.nix --command "cabal configure"

```

This project has now been setup to use the packages downloaded by Nix
instead of fetching them from Hackage, which is the default.

## TODO
Write the rest of this README.
