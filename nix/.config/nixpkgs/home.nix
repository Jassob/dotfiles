{ config, pkgs, ... }:

{
  # Allow non-free software (fonts, drivers, etc..):
  nixpkgs.config.allowUnfree = true;
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    # Applications
    ag
    alacritty
    bat
    browserpass
    coreutils
    drive
    dropbox-cli
    fasd
    file
    firefox
    fzf
    gnupg
    graphviz
    keybase
    light
    mosh
    networkmanager
    openssh
    pass
    pavucontrol
    playerctl
    pciutils
    ripgrep
    rofi-pass
    rustup
    spotify
    sshfs
    stow
    tdesktop
    tmux
    unzip
    usbutils
    vlc
    xclip

    # Haskell development
    haskellPackages.ghc
    haskellPackages.hlint
    haskellPackages.cabal-install
    haskellPackages.cabal2nix

    # For my XMonad setup
    blueman
    compton
    dunst
    feh
    i3lock
    imagemagick
    libnotify
    networkmanagerapplet
    pavucontrol
    pulsemixer
    rofi
    scrot
    sxhkd
    trayer
    haskellPackages.xmobar
    xorg.xmessage

    # Fonts to make available in X11 applications
    input-fonts

    # For my work
    gotools
    google-chrome

    # Emacs configuration is still maintained in emacs.nix
    (import /etc/nixos/packages/emacs.nix { inherit pkgs; })
  ];

  manual.html.enable = true;
  programs.git = {
    enable = true;
    userEmail = "jacob.t.jonsson@gmail.com";
    userName = "Jacob Jonsson";
  };

  services.gpg-agent = {
    enable = true;
    extraConfig = ''
      pinentry-program ${pkgs.pinentry}/bin/pinentry-gtk-2
      allow-emacs-pinentry
      allow-ssh-support
    '';
  };

  # Enable redshift
  services.redshift = {
    enable = true;
    temperature.day = 4000;
    temperature.night = 3500;
    longitude = "11.98";
    latitude = "57.68";
  };

  # Ensure fonts installed via Nix are picked up.
  fonts.fontconfig.enable = true;
}
