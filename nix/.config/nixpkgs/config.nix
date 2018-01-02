{ pkgs }: {

  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in with self; rec {

    myHaskellPackages = libProf: self: super:
    with pkgs.haskell.lib; let pkg = self.callPackage; in rec {

      ## Packages that I've modified and built locally
      filestore = dontCheck (doJailbreak (pkg ~/src/filestore {}));
      hasktags = pkg ./pkgs/hasktags.nix {};
      pandoc-citeproc = pkg ./pkgs/pandoc-citeproc-0.12.1.nix {};
      hakyll = doJailbreak (pkg ./pkgs/hakyll-4.9.8.0.nix { pandoc-citeproc = pandoc-citeproc; });

      ## Hackage overrides
      blaze-builder-enumerator = doJailbreak super.blaze-builder-enumerator;
      compressed = doJailbreak super.compressed;
      microlens = doJailbreak super.microlens;
      mime-mail = doJailbreak super.mime-mail;
      pipes-binary = doJailbreak super.pipes-binary;
      pipes-zlib = doJailbreak super.pipes-zlib;
      skylighting = dontCheck super.skylighting;
      time-recurrence = doJailbreak super.time-recurrence;
      Agda = dontHaddock super.Agda;
      cabal-install = doJailbreak super.cabal-install;
      gtk2hs-buildtools = doJailbreak super.gtk2hs-buildtools;
      hint = doJailbreak super.hint;

      hoogle_4_2_43 = super.hoogle_4_2_43.override {
        mkDerivation = args: super.mkDerivation (args // {
          enableSharedExecutables = false;
        });
      };
    };

    haskellPackages = super.haskellPackages.override {
      overrides = myHaskellPackages false;
    };

    profiledHaskellPackages = super.haskellPackages.override {
      overrides = myHaskellPackages true;
    };

    ghc82Env = pkgs.myEnvFun {
      name = "ghc82";
      buildInputs = with haskell821Packages; [
        (ghcWithHoogle (import ~/src/hoogle-local/package-list.nix))
        alex happy cabal-install
        ghc-core
        hlint
        hasktags
        ghc-mod
        djinn
      ];
    };

    # XMonad
    xmonad = xmonad-with-packages.override {
      packages = [ hPkgs.xmonad-contrib hPkgs.xmonad-extras hPkgs.xmobar ];
    };

    # Exposing custom packages
    hasktags = haskellPackages.hasktags;
    hakyll = haskellPackages.hakyll;
  };
}
