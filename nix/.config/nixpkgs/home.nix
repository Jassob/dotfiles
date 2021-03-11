{ config, pkgs, ... }:

let
  home_directory = builtins.getEnv "HOME";
  log_directory = "${home_directory}/.logs";
  tmp_directory = "/tmp";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  lib = pkgs.stdenv.lib;

in rec {
  # Allow non-free software (fonts, drivers, etc..):
  nixpkgs.config = import ./nixpkgs-config.nix;
  nixpkgs.overlays = [ (import (builtins.fetchTarball { url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz; })) ];

  home = {
    packages = with pkgs; [
      # Applications
      ag
      alacritty
      bat
      clang-tools # clang-format
      coreutils
      diffutils
      drive
      dropbox-cli
      entr
      fasd
      file
      firefox
      gnupg
      graphviz
      light
      mu
      nixpkgs-fmt
      networkmanager
      openssh
      pass
      pavucontrol
      playerctl
      pciutils
      ripgrep
      spotify
      sshfs
      steam
      stow
      tdesktop
      tmux
      unzip
      usbutils
      vlc
      xclip
      zathura

      # Haskell development
      haskellPackages.ghc
      haskellPackages.hlint
      haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.stack

      # Rust programming
      llvmPackages.libclang
      rustup

      # Appearance
      adapta-gtk-theme
      gnome3.adwaita-icon-theme

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
      scrot
      sxhkd
      trayer
      haskellPackages.xmobar
      xorg.xmessage

      # For my work
      google-chrome

      # Fonts
      corefonts # Microsoft free fonts
      dejavu_fonts
      fira-code
      fira-code-symbols
      hasklig
      inconsolata
      input-fonts
      iosevka
      ubuntu_font_family
      xits-math
      # TODO: package only iosevka-nerdfont
      nerdfonts
    ];

    file = {
      "${home_directory}/.gtkrc-2.0".text = ''
        # GENERATED CODE, DO NOT EDIT!
        # This file is generated by home-manager. Any changes made to this
        # file will be lost on next home-generation.

        include "${home_directory}/.gtkrc-2.0.mine"
        gtk-theme-name="Adapta-Eta"
        gtk-icon-theme-name="Adwaita"
        gtk-font-name="Sans 10"
        gtk-cursor-theme-name="Adwaita"
        gtk-enable-event-sounds=1
        gtk-enable-input-feedback-sounds=1
        gtk-xft-antialias=1
        gtk-xft-hinting=1
        gtk-xft-hintstyle="hintfull"
        gtk-xft-rgba="rgb"
      '';
    };
  };

  manual.html.enable = true;

  programs = {
    alacritty = {
      enable = true;
      settings = {
        window.decorations = "none";
        window.startup_mode = "Maximized";
        font.normal.family = "Iosevka Nerd Font";
        draw_bold_text_with_bright_colors = true;
        colors.primary = { background = "#1d2021"; foreground = "#ebdbbd"; };
        colors.normal = {
          black = "#000000";
          red = "#d54e53";
          green = "#b9ca4a";
          yellow = "#e6c547";
          blue = "#7aa6da";
          magenta = "#c397d8";
          cyan = "#70c0ba";
          white = "#eaeaea";
        };
        colors.bright = {
          black = "#666666";
          red = "#ff3334";
          green = "#9ec400";
          yellow = "#e7c547";
          blue = "#7aa6da";
          magenta = "#b77ee0";
          cyan = "#54ced6";
          white = "#ffffff";
        };
        url.modifiers = "Control";
        selection.save_to_clipboard = true;
        key_bindings = [
          { key = "Key0"; mods = "Control"; action = "ResetFontSize"; }
          { key = "Add"; mods = "Control"; action  = "IncreaseFontSize"; }
          { key = "Subtract"; mods = "Control"; action = "DecreaseFontSize"; }
        ];
      };
    };

    bash.profileExtra = ''
      # Setup GPG
      export GPG_TTY=$(tty)
      if ! pgrep -x "gpg-agent" > /dev/null; then
        ${pkgs.gnupg}/bin/gpgconf --launch gpg-agent
      fi
      export PATH=$HOME/.local/bin:$PATH
      export SSH_AUTH_SOCK=$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)
    '';

    command-not-found.enable = true;

    emacs = {
      enable = true;
      package = (pkgs.emacsWithPackagesFromUsePackage {
        config = ../../../emacs/init.el;
        extraEmacsPackages = epkgs: [ epkgs.use-package ];
      });
    };

    fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      delta.enable = true;
      userEmail = "jacob.t.jonsson@gmail.com";
      userName = "Jacob Jonsson";

      signing.key = "D822DFB8049AF39ADF43EA0A7E30B9B047F7202E";
      signing.signByDefault = true;

      aliases = {
        amend = "commit --amend -C HEAD";
        fp = "push --force-with-lease";
        rom = "!${pkgs.git}/bin/git fetch && ${pkgs.git}/bin/git rebase --autostash origin/master";
        sha = "rev-parse --short HEAD";
      };

      extraConfig = {
        core.whitespace = "trailing-space,space-before-tab";
        github.user = "Jassob";
        pull.rebase = true;
        rebase.autosquash = true;
        rerere.enabled = true;

        "url \"git@github.com:einride/\"".insteadOf = "https://github.com/einride/";
      };
    };

    gpg = {
      enable = true;
      settings = {
        keyserver = "hkps://hkps.pool.sks-keyservers.net";
        keyserver-options = "no-honor-keyserver-url";
        keyid-format = "0xlong";
      };
    };

    home-manager.enable = true;

    rofi = {
      enable = true;
      fullscreen = true;
      pass.enable = true;
      theme = "gruvbox-dark";
      package = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
    };

    zsh = {
      enable = true;
      defaultKeymap = "emacs";
      enableCompletion = true;
      enableAutosuggestions = true;
      history = {
        path = ".shell_history";
        ignoreDups = true;
        share = true;
      };

      sessionVariables = {
        # Download programs temporarily if missing
        NIX_AUTO_RUN = true;
        NIX_PATH = "$HOME/nix";
        # Only show the last two directories in current path
        PROMPT_DIRTRIM="2";
      };

      profileExtra = ''
        # Setup GPG
        export GPG_TTY=$(tty)
        if ! pgrep -x "gpg-agent" > /dev/null; then
            ${pkgs.gnupg}/bin/gpgconf --launch gpg-agent
        fi
        export SSH_AUTH_SOCK=$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)
        export PATH=$HOME/.local/bin:$PATH
        # Add keybindings
        bindkey '^T' transpose-chars
        bindkey '[T' transpose-words
        bindkey '^X^A^F' fzf-file-widget
        # Add keybindings from /etc/inputrc
        bindkey "\e[1~" beginning-of-line
        bindkey "\eOH" beginning-of-line
        bindkey "\e[H" beginning-of-line
        bindkey "\e[5~" beginning-of-history
        bindkey "\e[6~" end-of-history
        bindkey "\e[4~" end-of-line
        bindkey "\e[8~" end-of-line
        bindkey "\eOF" end-of-line
        bindkey "\e[F" end-of-line
        bindkey "\e[3~" delete-char
        bindkey "\e[2~" quoted-insert
        bindkey "\e[5C" forward-word
        bindkey "\e[1;5C" forward-word
        bindkey "\e[5D" backward-word
        bindkey "\e[1;5D" backward-word

        if [ -f "$HOME/.xsessionrc" ]; then . $HOME/.xsessionrc; fi
      '';

      shellAliases = {
        ls = "ls --color=auto";
        ll = "ls -alF";
        la = "ls -A";
        l = "ls -CF";
        # Keyboard layouts
        se = "setxkbmap -model emacs2 -option ctrl:nocaps,compose:rwin se";
        dv = "${pkgs.xorg.xkbcomp}/bin/xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mydvorak $DISPLAY";
        # Bluetooth
        sony-connect = "bluetoothctl connect 38:18:4C:D3:1A:20";
        sony-disconnect = "bluetoothctl disconnect 38:18:4C:D3:1A:20";
        # Emacs
        emproj = ''emacs --eval "(setq server-name \"$(basename $PWD)\")" --funcall server-start'';
        emacs-sync-gcal = ''
           emacs --batch --kill \\
                 --load ~/.emacs.d/init.el \\
                 --eval "(setq core/enabled-modules (quote (\"org\" \"org-gcal\")))" \\
                 --funcall core/load-modules \\
                 --eval "(setq org-directory \"/home/jassob/personal/\")" \\
                 --funcall org-gcal-sync'';
        dock = "~/.configurations/work-from-home.sh";
        dock-ask = "~/.configurations/work-from-home.sh -i";
        undock = "~/.configurations/laptop.sh";
      };

      initExtra = lib.mkBefore ''
        if [[ $TERM == dumb || $TERM == emacs || ! -o interactive ]]; then
            unsetopt zle
            unset zle_bracketed_paste
            export PS1='%m %~ $ '
        fi

        # Setup fasd
        eval "$(${pkgs.fasd}/bin/fasd --init posix-alias zsh-hook \
             zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install)"
        # Setup prompt
        PROMPT_COLOR=$(echo yellow blue green cyan magenta | ${pkgs.findutils}/bin/xargs ${pkgs.coreutils}/bin/shuf -n 1 -e)
        PROMPT="%B%F{$PROMPT_COLOR}%}%3~%f%b%f%F{white} %# %f";
      '';
    };
  };

  xdg = {
    enable = true;
    configHome = "${home_directory}/.config";
    dataHome = "${home_directory}/.local/share";
    configFile."${home_directory}/.tmux.conf".source = ../../../xdg-configs/.tmux.conf;
    configFile."${home_directory}/.emacs".source = ../../../emacs/init.el;
    configFile."dunst".source = ../../../xdg-configs/.config/dunst;
    configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;
  };

  services.emacs = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    extraConfig = ''
      pinentry-program ${pkgs.pinentry}/bin/pinentry
      allow-emacs-pinentry
      enable-ssh-support
    '';
  };

  # Mail synchronization
  services.mbsync = {
    enable = true;
    configFile = "${xdg.configHome}/mbsyncrc";
    postExec = "${pkgs.mu}/bin/mu index";
  };
  xdg.configFile."mbsyncrc".text = ''
    IMAPAccount personal
    Host imap.gmail.com
    Port 993
    User jacob.t.jonsson@gmail.com
    PassCmd "${pkgs.pass}/bin/pass notes/gmail-app-password"
    SSLType IMAPS
    SSLVersions TLSv1.2
    CertificateFile ${ca-bundle_crt}

    IMAPStore personal-remote
    Account personal

    MaildirStore personal-local
    Path ~/.mail/personal/
    Inbox ~/.mail/personal/Inbox

    Channel personal-inbox
    Master :personal-remote:
    Slave :personal-local:
    Create Slave
    Expunge Both
    SyncState *

    Channel personal-all
    Master :personal-remote:"[Gmail]/All Mail"
    Slave :personal-local:"all"
    Create Slave
    Expunge Both
    SyncState *

    Channel personal-sent
    Master :personal-remote:"[Gmail]/Sent Mail"
    Slave :personal-local:"sent"
    Create Slave
    Expunge Both
    SyncState *

    Channel personal-starred
    Master :personal-remote:"[Gmail]/Starred"
    Slave :personal-local:"starred"
    Create Slave
    Expunge Both
    SyncState *

    Channel personal-trash
    Master :personal-remote:"[Gmail]/Trash"
    Slave :personal-local:"trash"
    Create Slave
    Expunge Both
    SyncState *

    Group personal
    Channel personal-inbox
    Channel personal-all
    Channel personal-sent
    Channel personal-starred
    Channel personal-trash

    Create Slave
    SyncState *
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
