# Autostart if session is XMonad
if [[ -v DESKTOP_SESSION ]] && [[ "$DESKTOP_SESSION" == "none+xmonad" ]]; then
    source ~/dotfiles/jassob/autostart.sh
fi
