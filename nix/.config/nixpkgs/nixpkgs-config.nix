{ pkgs }: {

  allowUnfree = true;
  overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
    (self: super: {
      xmonad = super.xmonad-with-packages.override {
         packages = self: with self; [ xmonad-contrib xmonad-extras xmobar ];
      };
    })
  ];
}
