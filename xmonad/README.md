# xmonad

This is my XMonad config, with an attempt of making it work in NixOS.

## Getting started

1. Clone the repo somewhere and symlink this folder (xmonad/) to
   ~/.xmonad or ~/.config/xmonad/.
1. Symlink ./.xmonad/build.nixos -> ~/.xmonad/build

This project has now been setup to use the packages downloaded by Nix
instead of fetching them from Hackage, which is the default.

## Getting started on Ubuntu

We need to install some dependencies:

```bash
sudo apt install xmonad libghc-pango-dev libasound2-dev
```

Symlink ../.xmonad/build.ubuntu -> ~/.xmonad/build

## TODO
Write the rest of this README.
