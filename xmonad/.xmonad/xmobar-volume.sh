#!/usr/bin/env sh -eo

# Extract amixer Master information
VOL=$(pulsemixer --get-volume | awk '{ print $1 }')
MUTE=$(pulsemixer --get-mute)

# Add the muted line if the Master device is off (muted)
if [ "${MUTE}" = "1" ]; then
    ICON="婢"
elif [ "${VOL}" -le 15 ]; then
    ICON="奄"
elif [ "${VOL}" -le 50 ]; then
    ICON="奔"
elif [ "${VOL}" -le 75 ]; then
    ICON="墳"
else
    ICON=""
fi

echo "${ICON} ${VOL}%"
