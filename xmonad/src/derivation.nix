{ mkDerivation, array, base, bytestring, containers, data-default
, data-default-class, data-default-instances-containers
, data-default-instances-dlist, data-default-instances-old-locale
, deepseq, directory, dlist, extensible-exceptions, filepath
, ghc-prim, integer-gmp, mtl, old-locale, old-time, process, random
, rts, setlocale, stdenv, time, transformers, unix, utf8-string
, X11, X11-xft, xmonad, xmonad-contrib, xmobar, lib
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
    utf8-string X11 X11-xft xmonad xmonad-contrib xmobar
  ];
  license = lib.licenses.bsd3;
}
