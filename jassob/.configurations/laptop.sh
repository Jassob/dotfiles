#!/bin/sh
xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x1432 --rotate normal \
       --output HDMI-2 --off \
       --output HDMI-1 --off \
       --output DP-1 --off \
       --output DP-2 --off

# Set background
~/.fehbg

# Set keyboard
setxkbmap -model emacs2 -option ctrl:nocaps,compose:rwin se

# Rerun sxhkd
pkill sxhkd
run sxhkd
