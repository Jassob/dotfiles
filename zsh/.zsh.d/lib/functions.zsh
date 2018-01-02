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
