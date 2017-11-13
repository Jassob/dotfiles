#!/usr/bin/bash

# Extract amixer Master information
LINE=$(amixer get Master | grep -E "[0-9]+%")
VOL=$(echo $LINE | egrep -o "[0-9]+%" | uniq)

# Add the muted line if the Master device is off (muted)
if [[ $LINE =~ "off" ]] ; then
    echo "muted (${VOL})"
else
    echo ${VOL}
fi
