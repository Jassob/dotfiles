self: super:
{
  myHaskellPackages = libProf: slf: sup:
  with super.haskell.lib; let pkg = slf.callPackage; in rec {
    ## Packages that I've modified and built locally
    filestore = dontCheck (doJailbreak (pkg ~/src/filestore {}));
    hasktags = pkg ../pkgs/hasktags.nix {};
    pandoc-citeproc = pkg ../pkgs/pandoc-citeproc-0.12.1.nix {};
    hakyll = doJailbreak (pkg ../pkgs/hakyll-4.9.8.0.nix { pandoc-citeproc = pandoc-citeproc; });
    bnfc = dontCheck (pkg ../pkgs/bnfc-2.8.1.nix {});

    ## Hackage overrides
    blaze-builder-enumerator = doJailbreak sup.blaze-builder-enumerator;
    compressed = doJailbreak sup.compressed;
    microlens = doJailbreak sup.microlens;
    mime-mail = doJailbreak sup.mime-mail;
    pipes-binary = doJailbreak sup.pipes-binary;
    pipes-zlib = doJailbreak sup.pipes-zlib;
    skylighting = dontCheck sup.skylighting;
    time-recurrence = doJailbreak sup.time-recurrence;
    Agda = dontHaddock sup.Agda;
    cabal-install = doJailbreak sup.cabal-install;
    gtk2hs-buildtools = doJailbreak sup.gtk2hs-buildtools;
    hint = doJailbreak sup.hint;

    hoogle = sup.hoogle.override {
      mkDerivation = args: sup.mkDerivation (args // {
        enableSharedExecutables = false;
      });
    };
  };

  # Generates environments with local haskell packages
  haskellEnvFun = { withHoogle ? false, compiler ? null, packages ? null, name }:
  let hp = if compiler != null
           then super.haskell.packages.${compiler}
           else self.haskellPackages;
      paths = if packages != null
              then packages
              else (ghcWith (import ~/src/hoogle-local/package-list.nix));
     ghcWith = if withHoogle then hp.ghcWithHoogle else hp.ghcWithPackages;
  in super.buildEnv {
    name = name;
    paths = paths;
  };

  # Example Haskell Environment
  # haskellEnvHoogle = self.haskellEnvFun {
  #   name = "haskellEnvHoogle";
  #   withHoogle = true;
  #   packages = with self.haskellPackages; [ pipes-async ];
  # };

  profiledHaskellPackages = super.haskellPackages.override {
    overrides = self.myHaskellPackages true;
  };

  haskellPackages = super.haskellPackages.override {
    overrides = self.myHaskellPackages true;
  };
}
