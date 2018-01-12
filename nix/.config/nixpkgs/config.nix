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
      bnfc = dontCheck (pkg ./pkgs/bnfc-2.8.1.nix {});

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

      hoogle = super.hoogle.override {
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

    # Generates environments with local haskell packages
    haskellEnvFun = { withHoogle ? false, compiler ? null, name }:
      let hp = if compiler != null
                  then super.haskell.packages.${compiler}
                  else haskellPackages;

          ghcWith = if withHoogle
                      then hp.ghcWithHoogle
                      else hp.ghcWithPackages;

      in super.buildEnv {
       name = name;
       paths = [(ghcWith (import ~/src/hoogle-local/package-list.nix))];
      };

    # Haskell environment with Hoogle
    haskellEnvHoogle = haskellEnvFun {
      name = "haskellEnvHoogle";
      withHoogle = true;
    };

    # Haskell environment without Hoogle
    haskellEnv = haskellEnvFun {
      name = "haskellEnv";
      withHoogle = false;
    };

    ghc82Env = pkgs.myEnvFun {
      name = "ghc82";
      buildInputs = with haskellPackages; [
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

    feed-the-beast = self.callPackage ./pkgs/feed-the-beast.nix {};
  };
}
