# Select Go version and update environment to use
#
# Usage: use-go 1.13      -- Uses Go version 1.13
#        use-go 1.14      -- Uses Go version 1.14
#        use-go           -- Uses default Go version (currently 1.14)
function use-go() {
    GO_VERSION=${1:-1.14} # Use Go 1.14 if no argument is given
    if [[ -z $OLDPATH ]]; then
	export OLDPATH=$PATH
    fi
    export GOROOT="$HOME/.local/share/devtools/go/${GO_VERSION}/go"
    export GOPATH=$HOME/go
    export PATH=$GOPATH/bin:$OLDPATH
    export PATH=$GOROOT/bin:$OLDPATH
    export GOPRIVATE=github.com/einride/*
    echo "Go environment set up for Go $GO_VERSION"
}
