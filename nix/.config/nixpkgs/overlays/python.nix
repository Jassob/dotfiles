self: super:
{
  # Custom python packages
  myPythonPackages = rec {
    click_6_6 = with super.pythonPackages; callPackage ../pkgs/click-6.6.nix { inherit buildPythonPackage fetchPypi pytest; };
    arrow_0_10_0 = with super.pythonPackages; callPackage ../pkgs/arrow-0.10.0.nix
      { inherit buildPythonPackage fetchPypi nose chai simplejson dateutil backports_functools_lru_cache; };
    gitlint = with super.pythonPackages; callPackage ../pkgs/gitlint.nix {
      inherit buildPythonPackage fetchPypi sh pkgconfig;
      click = click_6_6;
      arrow = arrow_0_10_0;
    };
  };
}