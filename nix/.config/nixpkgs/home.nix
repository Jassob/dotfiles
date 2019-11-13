{ config, pkgs, ... }:

let
  home_directory = builtins.getEnv "HOME";
  log_directory = "${home_directory}/.logs";
  tmp_directory = "/tmp";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  lib = pkgs.stdenv.lib;

  # Custom packages
  emacs = (import /etc/nixos/packages/emacs.nix { inherit pkgs; });

in rec {
  # Allow non-free software (fonts, drivers, etc..):
  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = false;
    allowUnsupportedSystem = false;
  };

  home = {
    packages = with pkgs; [
      # Applications
      ag
      alacritty
      bat
      coreutils
      diffutils
      drive
      dropbox-cli
      emacs
      entr
      fasd
      file
      firefox
      fzf
      gnupg
      graphviz
      keybase
      light
      mosh
      mu
      networkmanager
      openssh
      pass
      pavucontrol
      playerctl
      pciutils
      ripgrep
      rofi-pass
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
      input-fonts

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

      # For my work
      gotools
      google-chrome
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
    git = {
      enable = true;
      userEmail = "jacob.t.jonsson@gmail.com";
      userName = "Jacob Jonsson";

      signing = {
        key = "D822DFB8049AF39ADF43EA0A7E30B9B047F7202E";
        signByDefault = true;
      };

      aliases = {
        amend = "commit --amend -C HEAD";
        authors = ''
          !${pkgs.git}/bin/git log --pretty=format:%aN \
                     | ${pkgs.coreutils}/bin/sort \
                     | ${pkgs.coreutils}/bin/uniq -c \
                     | ${pkgs.coreutils}/bin/sort -rn'';
        changes = "diff --name-status -r";
        cp = "cherry-pick";
        ls-ignored = "ls-files --exclude-standard --ignored --others";
        rom = ''
          !${pkgs.git}/bin/git fetch \
                     && ${pkgs.git}/bin/git rebase --autostash origin/master'';
      };

      extraConfig = {
        core.whitespace = "trailing-space,space-before-tab";
        commit.gpgsign = true;
        github.user = "Jassob";
        pull.rebase = true;
        rebase.autosquash = true;
        rerere.enabled = true;

        color = {
          status = "auto";
          diff = "auto";
          branch = "auto";
          interactive = "auto";
          ui = "auto";
          sh = "auto";
        };

        "url \"git@github.com:einride/\"".insteadOf =
          "https://github.com/einride/";
      };
    };

    home-manager.enable = true;

    browserpass = {
      enable = true;
      browsers = [ "firefox" "chrome" ];
    };

    zsh = {
      enable = true;
      defaultKeymap = "emacs";
      dotDir = ".zsh.d";
      enableCompletion = true;
      enableAutosuggestions = true;
      history = {
        size = 50000;
        save = 500000;
        path = ".shell_history";
        ignoreDups = true;
        share = true;
      };

      sessionVariables = {
        # Download programs temporarily if missing
        NIX_AUTO_RUN = true;
      };

      profileExtra = ''
        # Setup GPG
        export GPG_TTY=$(tty)
        if ! pgrep -x "gpg-agent" > /dev/null; then
            ${pkgs.gnupg}/bin/gpgconf --launch gpg-agent
        fi
        export PATH=$HOME/.local/bin:$PATH
      '';

      shellAliases = {
        ls = "ls --color=auto";
        ll = "ls -alF";
        la = "ls -A";
        l = "ls -CF";
        # Keyboard layouts
        se = "setxkbmap -model emacs2 -option ctrl:nocaps,compose:rwin se";
        dv =
        "${pkgs.xorg.xkbcomp}/bin/xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mydvorak $DISPLAY 2> /dev/null";
        # Bluetooth
        plattan-connect = "bluetoothctl connect 5C:EB:68:45:78:6D";
        plattan-disconnect = "bluetoothctl disconnect 5C:EB:68:45:78:6D";
        # Emacs
        emproj = ''
          emacs --eval "(setq server-name \"$(basename $PWD)\")" \
                --funcall server-start'';
      };

      initExtra = lib.mkBefore ''
        export SSH_AUTH_SOCK=$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)
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
        PROMPT_DIRTRIM="2";
        RPROMPT="";
      '';
    };

    fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };

    alacritty = {
      enable = true;
      settings = {
        window = {
          decorations = "none";
          startup_mode = "Maximized";
        };
        font = {
          normal.family = "Iosevka";
          size = 10.0;
          use_thin_strokes = true;
        };
        draw_bold_text_with_bright_colors = true;
        colors = {
          primary = {
            background = "0x1d2021";
            foreground = "0xebdbbd2";
          };
          normal = {
            black = "0x000000";
            red = "0xd54e53";
            green = "0xb9ca4a";
            yellow = "0xe6c547";
            blue = "0x7aa6da";
            magenta = "0xc397d8";
            cyan = "0x70c0ba";
            white = "0xeaeaea";
          };
          bright = {
            black = "0x666666";
            red = "0xff3334";
            green = "0x9ec400";
            yellow = "0xe7c547";
            blue = "0x7aa6da";
            magenta = "0xb77ee0";
            cyan = "0x54ced6";
            white = "0xffffff";
          };
        };
        url.modifiers = "Control";
        selection = {
          semantic_escape_chars = ",|`|:\"' ()[]{}<>";
          save_to_clipboard = true;
        };
        dynamic_title = true;
        cursor = {
          style = "Block";
          unfocused_hollow = true;
        };
        live_config_reload = true;
        key_bindings = [
          { key = "Insert"; mods = "Shift"; action = "PasteSelection"; }
          { key = "Key0"; mods = "Control"; action = "ResetFontSize"; }
          { key = "Add"; mods = "Control"; action  = "IncreaseFontSize"; }
          { key = "Subtract"; mods = "Control"; action = "DecreaseFontSize"; }
        ];
      };
    };
    command-not-found.enable = true;
    gpg = {
      enable = true;
      settings = {
        keyserver = "hkps://hkps.pool.sks-keyservers.net";
        keyserver-options = "no-honor-keyserver-url";
        keyid-format = "0xlong";
      };
    };
  };

  xdg = {
    enable = true;
    configHome = "${home_directory}/.config";
    dataHome = "${home_directory}/.local/share";
    cacheHome = "${home_directory}/.cache";

    configFile."msmtp".text = ''
      defaults
      tls on
      tls_starttls on
      trl_trust_file ${ca-bundle_crt}

      account gmail
      host smtp.gmail.com
      port 587
      auth on
      user ${programs.git.userEmail}
      passwordeval ${pkgs.pass}/bin/pass notes/gmail-app-password
      from ${programs.git.userEmail}
      logfile ${log_directory}/msmtp.log
    '';

    configFile."mbsyncrc".text = ''
      IMAPAccount personal
      # Address to connect to
      Host imap.gmail.com
      Port 993
      User jacob.t.jonsson@gmail.com
      PassCmd "${pkgs.pass}/bin/pass notes/gmail-app-password"
      SSLType IMAPS
      SSLVersions TLSv1.2
      # The following line should work. If get certificate errors,
      # uncomment the two following lines and read the "Troubleshooting" section.
      CertificateFile /etc/ssl/certs/ca-bundle.crt

      IMAPStore personal-remote
      Account personal

      MaildirStore personal-local
      Subfolders Verbatim
      # The trailing "/" is important
      Path ~/.mail/personal/
      Inbox ~/.mail/personal/Inbox

      Channel personal
      Master :personal-remote:
      Slave :personal-local:
      # Exclude everything under the internal [Gmail] folder, except the interesting folders
      #Patterns * ![Gmail]* "[Gmail]/Skickat" "[Gmail]/Stjärnmärkt" "[Gmail]/Alla mail" "[Gmail]/Utkast"
      # Or include everything
      Patterns * "![Gmail]/Skr&AOQ-ppost"
      # Automatically create missing mailboxes, both locally and on the server
      Create Both
      # Save the synchronization state files in the relevant directory
      SyncState *
    '';
  };

  services.gpg-agent = {
    enable = true;
    extraConfig = ''
      pinentry-program ${pkgs.pinentry}/bin/pinentry-gtk-2
      allow-emacs-pinentry
      enable-ssh-support
    '';
  };

  services.mbsync = {
    enable = true;
    configFile = "${xdg.configHome}/mbsyncrc";
    postExec = "${pkgs.mu}/bin/mu index --maildir=~/.mail/personal";
  };

  services.keybase.enable = true;

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
