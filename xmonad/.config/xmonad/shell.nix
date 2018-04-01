{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bytestring, containers
      , data-default, data-default-class
      , data-default-instances-containers, data-default-instances-dlist
      , data-default-instances-old-locale, deepseq, directory, dlist
      , extensible-exceptions, filepath, ghc-prim, integer-gmp, mtl
      , old-locale, old-time, process, random, rts, setlocale, stdenv
      , time, transformers, unix, utf8-string, X11, X11-xft, xmonad
      , xmonad-contrib
      }:
      mkDerivation {
        pname = "xmonad-config";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array base bytestring containers data-default data-default-class
          data-default-instances-containers data-default-instances-dlist
          data-default-instances-old-locale deepseq directory dlist
          extensible-exceptions filepath ghc-prim integer-gmp mtl old-locale
          old-time process random rts setlocale time transformers unix
          utf8-string X11 X11-xft xmonad xmonad-contrib
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
