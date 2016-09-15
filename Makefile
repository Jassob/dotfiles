SHELL=/bin/bash

.PHONY: all tmux

all: emacs tmux xinitrc xresources

emacs:
	if [ -f $(HOME)/.emacs ]; then \
		echo "Backup old Emacs config (~/.emacs -> ~/.emacs.bak)"; \
		mv ~/.emacs ~/.emacs.bak; \
	fi
	if [ -d $(HOME)/.emacs.d ]; then \
		echo "Backup old Emacs config dir (~/.emacs.d/ -> ~/.emacs.d.bak/)"; \
		mv ~/.emacs.d ~/.emacs.d.bak; \
	fi
	echo "Installing Emacs configs..."
	cp -r emacs.d ~/.emacs.d
	echo "Done!"

tmux:
	if [ -f $(HOME)/.tmux.conf ]; then \
		echo "Backup old tmux config (~/.tmux.conf -> ~/.tmux.bak)"; \
		mv ~/.tmux ~/.tmux.bak; \
	fi
	if [ -d $(HOME)/.tmux.d ]; then \
		echo "Backup old tmux config dir (~/.tmux.d/ -> ~/.tmux.d.bak/"); \
		mv ~/.tmux.d ~/.tmux.d.bak; \
	fi
	echo "Installing tmux configs..."
	cp -r tmux/* ~/
	echo "Done!"

local-scripts:
	if ! [ -d $(HOME)/.local/bin ]; then \
		mkdir -p $(HOME)/.local/bin; \
	fi
	echo "Installing local scripts..."
	cp .local/bin/* $(HOME)/.local/bin;
	echo "Done!"
