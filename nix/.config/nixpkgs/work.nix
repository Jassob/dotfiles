{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    google-chrome
    google-cloud-sdk
    gnumake
    gcc
    llvmPackages_10.libclang
    slack
    # Network
    tcpdump
    wireshark
    nomachine-client
    # NodeJS
    nodejs
    yarn
    # VPN
    openvpn
    wireguard
    # Rust
    rust-analyzer
    rustup
    # Golang
    delve
    go_1_16
    gopls
  ];

  home.sessionVariables = {
    LIBCLANG_PATH = lib.makeLibraryPath [pkgs.llvmPackages_10.libclang];
  };

  programs.git.includes = [
    {
      condition = "gitdir:~/einride/";
      contents = {
        user = {
          email = "jacob.jonsson@einride.tech";
          signingKey = "CCBF85D61123CAB9AEE87833B528CA8CCD4019F8";
        };
      };
    }
  ];

  programs.go = {
    enable = true;
    package = pkgs.go_1_16;
  };

  programs.jq.enable = true;

  programs.kakoune = {
    enable = true;
    plugins = [pkgs.kakounePlugins.kak-fzf pkgs.kakounePlugins.kak-lsp];
  };
}
