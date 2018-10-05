{ pkgs }: {

  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in with self; rec {

    # XMonad
    xmonad = xmonad-with-packages.override {
      packages = self: with self; [ xmonad-contrib xmonad-extras xmobar ];
    };

    # Pidgin
    pidgin-with-plugins = super.pidgin-with-plugins.override {
      plugins = [ pkgs.purple-facebook pkgs.telegram-purple ];
    };

    # Exposing custom packages
    hasktags = haskellPackages.hasktags;
    hakyll = haskellPackages.hakyll;
  };
}
