# Functions and scripts for my work setup

# Go Test, run go test -race -cover -count=1
# TODO: Find out why this is bork
function gt() {
    set -o pipefail
    NUM=${1:-10}
    PACKAGES=${2:-./...}
    shift
    shift
    if [[ $# -gt 0 ]]; then TESTFLAGS="$@"; else TESTFLAGS="-race -cover -count=1"; fi
    FAILED=0
    for i in $(seq 1 $NUM); do
	echo "Round $i..."
	go test $(echo $TESTFLAGS) $(echo $PACKAGES) || FAILED=$((FAILED + 1))
    done
    echo "$FAILED tests out of $NUM failed"
}

function git-cleanup-branches() {
    local REMOVED=0
    for b in $(git branch | grep -v \* | grep -v master); do
	git branch -r | grep ${b} > /dev/null || REMOVED=$((REMOVED + 1)); git branch -D ${b}
    done
    echo "Cleaned up $REMOVED branches"
}

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
