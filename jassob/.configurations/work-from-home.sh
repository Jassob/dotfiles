#!/bin/sh

if [ "$1" = "-i" ]; then
    arandr
else
    xrandr --output eDP-1 --mode 1920x1080 --pos 1920x256 --rotate normal \
	   --output HDMI-2 --primary --mode 1920x1080 --pos 0x0 --rotate normal \
	   --output HDMI-1 --off \
	   --output DP-1 --off \
	   --output DP-2 --off
fi

# Set background
~/.fehbg

# Set keyboard
xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mydvorak $DISPLAY 2> /dev/null

# Rerun sxhkd
pkill sxhkd
run sxhkd

# Rerun trayer
pkill trayer
run ~/.xmonad/xmobar-trayer.sh
