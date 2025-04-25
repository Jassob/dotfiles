{
  description = "display-hs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:

    let system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
        inputs = [
          (pkgs.haskellPackages.ghcWithPackages (hPkgs: [
            hPkgs.X11
            hPkgs.cabal-install
            hPkgs.haskell-language-server
          ]))
        ];
    in {
      packages.x86_64-linux.display = pkgs.haskellPackages.developPackage {
        root = ./.;
      };

      packages.x86_64-linux.default = self.packages.x86_64-linux.display;

      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = inputs;
      };
    };
}
