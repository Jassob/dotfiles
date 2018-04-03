#!/usr/bin/bash

# Extract amixer Master information
VOL=$(pamixer --get-volume)
MUTE=$(pamixer --get-mute)

# Add the muted line if the Master device is off (muted)
if [[ $MUTE =~ "true" ]] ; then
    echo "muted (${VOL}%)"
else
    echo "${VOL}%"
fi
