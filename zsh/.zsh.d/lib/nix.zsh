env-type () {
	envtype="$1"
	shift
	nix-shell -Q -p $envtype "$@"
}

haskell-env () {
	env-type "haskellEnv" "$@"
}

haskell-env-hoogle () {
	env-type "haskellEnvHoogle" "$@"
}

nix-search () {
    echo "Searching for \"$1\" in default attribute set..."
    nix-env -qaP | grep -i "$1"
}

nix-search-haskell-packages () {
    echo "Searching for \"$1\" in attribute haskellPackages..."
    list-haskell-packages | grep -i "$1"
}

list-haskell-packages () {
    nix-env -qaP -A haskellPackages
}

list-nixpkgs () {
    nix-env -qaP
}

list-python-packages () {
    nix-env -qaP -A python3Packages
}

list-python2-packages () {
    nix-env -qaP -A pythonPackages
}

# Link ~/.nix-defexpr -> ~/nixpkgs
rm -r ~/.nix-defexpr
ln -s ~/nixpkgs ~/.nix-defexpr

export NIX_PATH=$HOME:nixos-config=/etc/nixos/configuration.nix
