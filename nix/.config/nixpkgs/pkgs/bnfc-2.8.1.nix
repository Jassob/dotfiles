{ mkDerivation, alex, array, base, containers, deepseq, directory
, doctest, filepath, happy, hspec, HUnit, mtl, pretty, process
, QuickCheck, stdenv, temporary
}:
mkDerivation {
  pname = "BNFC";
  version = "2.8.1";
  sha256 = "2c1bea5c034483813091eea0ea5c830fdde8fedd31b1fc021ea69823b30a5920";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base ];
  executableHaskellDepends = [
    array base containers deepseq directory filepath mtl pretty process
  ];
  executableToolDepends = [ alex happy ];
  testHaskellDepends = [
    array base containers deepseq directory doctest filepath hspec
    HUnit mtl pretty process QuickCheck temporary
  ];
  homepage = "http://bnfc.digitalgrammars.com/";
  description = "A compiler front-end generator";
  license = stdenv.lib.licenses.gpl2;
}
