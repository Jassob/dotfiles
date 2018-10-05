self: super:
{
  mypkgs.feed-the-beast = super.callPackage ../pkgs/feed-the-beast.nix { mesa = super.mesa; };

  mypkgs.lxappearance = super.lxappearance.overrideAttrs(old: rec {
    name = "lxappearance-0.6.2";
    src = super.fetchurl {
      url = "mirror://sourceforge/project/lxde/LXAppearance/${name}.tar.xz";
      sha256 = "07r0xbi6504zjnbpan7zrn7gi4j0kbsqqfpj8v2x94gr05p16qj4";
    };
  });
}
