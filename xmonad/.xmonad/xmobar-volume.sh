#!/usr/bin/env sh -eo

VOL=$(pulsemixer --get-volume | awk '{ print $1 }')
MUTE=$(pulsemixer --get-mute)

if bluetoothctl info | grep -q "UUID: Headset"; then
    if [ "${MUTE}" = "1" ]; then
	ICON="󰟎"
    else
	ICON="󰋋"
    fi
elif [ "${MUTE}" = "1" ]; then
    ICON="󰝟"
elif [ "${VOL}" -le 15 ]; then
    ICON="󰕿"
elif [ "${VOL}" -le 50 ]; then
    ICON="󰖀"
elif [ "${VOL}" -le 75 ]; then
    ICON="󰕾"
else
    ICON=""
fi

echo "${ICON} ${VOL}%"
