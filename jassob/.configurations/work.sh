#!/bin/sh
xrandr --output eDP-1 --mode 1920x1080 --pos 0x1432 --rotate normal \
       --output HDMI-2 --primary --mode 3840x2160 --pos 1920x0 --rotate normal \
       --output HDMI-1 --off \
       --output DP-1 --off \
       --output DP-2 --off

# Set background
~/.fehbg

# Set keyboard
xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mydvorak $DISPLAY 2> /dev/null

# Rerun sxhkd
pkill sxhkd
run sxhkd
