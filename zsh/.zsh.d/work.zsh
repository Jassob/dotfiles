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

# Get a list of commit messages and their hash
#
# Usage: git-new-release-note v0.5.3   -- Uses v0.5.3 as last tag
# Usage: git-new-release-note          -- Prompts the user for a tag
function git-new-release-note() {
    if [ -n "$1" ]; then
	LAST_TAG="$1"
    else
	read "LAST_TAG?Please enter the latest current Git tag (or release): "
    fi
    git log --pretty='format:- %s %h' ${LAST_TAG}..HEAD
}
