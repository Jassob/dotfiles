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

export NIX_PATH=$HOME/nix:nixos-config=/etc/nixos/configuration.nix
# Use nix-shell to run a missing program in a temporary environment
export NIX_AUTO_RUN=true

# Use NixOS command-not-found script
command_not_found_handler() {
    if [ ! -f /run/current-system/sw/bin/command-not-found ]; then
	echo command not found: $1
	exit 127
    fi
    command-not-found $1
}
