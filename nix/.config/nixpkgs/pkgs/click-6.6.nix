{ stdenv, pkgs, buildPythonPackage, fetchPypi, pytest }:

buildPythonPackage rec {
  pname = "click";
  version = "6.6";
  name = "${pname}-${version}";

  src = fetchPypi {
    inherit pname version;
    sha256 = "cc6a19da8ebff6e7074f731447ef7e112bd23adf3de5c597cf9989f2fd8defe9";
  };

  buildInputs = [ pytest ];

  checkPhase = ''
    py.test tests
  '';

  # https://github.com/pallets/click/issues/823
  doCheck = false;

  meta = {
    homepage = http://click.pocoo.org/;
    description = "Create beautiful command line interfaces in Python";
    longDescription = ''
      A Python package for creating beautiful command line interfaces in a
      composable way, with as little code as necessary.
    '';
    license = pkgs.licenses.bsd3;
  };
}
