{ stdenv, buildPythonPackage, fetchPypi, sh, arrow, click, pkgconfig }:

buildPythonPackage rec {
  pname = "gitlint";
  version = "0.10.0";
  name = "${pname}-${version}";

  src = fetchPypi {
    inherit pname version;
    sha256 = "8eb9e6839fe1a1f7d9f9e9b9b7a878a76ed5a73fe5e00988d1c78784914c6055";
  };

  nativeBuildInputs = [ pkgconfig sh click ];
  propagatedBuildInputs = [ click arrow ];

  doCheck = false;

  meta = with stdenv.lib; {
    description = "Git commit message linter written in python, checks your commit messages for style.";
    homepage = "https://github.com/jorisroovers/gitlint";
    license = licenses.mit;
  };
}
