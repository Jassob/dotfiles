# Autostart programs that exists in ~/.config/autostart, if they are
# not already running

DIR="$HOME/.config/autostart/"

# Exit with success if folder does not exist.
if [ ! -d "$DIR" ] ; then exit 0 ; fi

user=$(whoami)

for desktopFile in $DIR*.desktop ; do
    # Strip .desktop suffix and path
    prog="$(basename ${desktopFile%.desktop})"

    noMatches=$(ps -U "$user" -u "$user" | grep -i $prog | wc -l)
    # Start if no matches were found
    if [ $noMatches -lt 1 ]; then
	echo "Starting $prog"
	cmdLine=$(cat "$desktopFile" | grep Exec)
	${cmdLine#Exec=} &
	disown %1
    else
	echo "Not starting $prog"
    fi
done
