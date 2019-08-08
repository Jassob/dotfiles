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
