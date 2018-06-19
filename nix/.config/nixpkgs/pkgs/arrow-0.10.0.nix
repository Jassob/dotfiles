{ stdenv, buildPythonPackage, fetchPypi, nose, chai,
simplejson, dateutil, backports_functools_lru_cache }:

buildPythonPackage rec {
  pname = "arrow";
  version = "0.10.0";
  name = "${pname}-${version}";

  src = fetchPypi {
    inherit pname version;
    sha256 = "805906f09445afc1f0fc80187db8fe07670e3b25cdafa09b8d8ac264a8c0c722";
  };

  checkPhase = ''
    nosetests --cover-package=arrow
  '';

  checkInputs = [ nose chai simplejson ];
  propagatedBuildInputs = [ dateutil backports_functools_lru_cache ];

  postPatch = ''
    substituteInPlace setup.py --replace "==1.2.1" ""
  '';

  meta = with stdenv.lib; {
    description = "Python library for date manipulation";
    license     = "apache";
    maintainers = with maintainers; [ thoughtpolice ];
  };
}
