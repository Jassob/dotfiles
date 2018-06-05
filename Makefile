usual: emacs jassob xdg-configs xmonad xresources zsh

emacs:
	if [ -d "${HOME}/.emacs.d/" ]; then mv "${HOME}/.emacs.d" "${HOME}/.emacs.d.bak"; fi
	stow emacs
	if [ -f "${HOME}/.emacs" ]; then mv "${HOME}/.emacs" "${HOME}/.emacs.bak"; fi
	echo "Emacs installed."
	echo "Insert this into your crontab to make Emacs periodically sync org-gcal:"
	echo "######################################"
	tail -n +3 "${HOME}/dotfiles/jassob/.local/bin/emacs-sync-gcal"
	echo "######################################"

conkeror:
	stow conkeror

jassob:
	stow jassob

nix:
	stow nix

stumpwm:
	stow stumpwm

xmonad:
	stow xmonad

xdg-configs:
	stow xdg-configs

xresources:
	stow xresources

zsh:
	stow zsh

cleanbackups:
	find ~/ -maxdepth 1 -name \*.bak -exec rm {} +

.PHONY: usual emacs conkeror jassob nix stumpwm xmonad xdg-configs xresources zsh cleanbackups
