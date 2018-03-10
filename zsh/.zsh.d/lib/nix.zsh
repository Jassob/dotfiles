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

nix-search() {
	echo "Searching..."
	nix-env -qaP | grep -i "$1"
}

# Link ~/.nix-defexpr -> ~/nixpkgs
rm -r ~/.nix-defexpr
ln -s ~/nixpkgs ~/.nix-defexpr

export NIX_PATH=$HOME:nixos-config=/etc/nixos/configuration.nix
