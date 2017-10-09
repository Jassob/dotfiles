SHELL=/bin/bash

.PHONY: all tmux

ifndef VERBOSE
.SILENT:
endif

all: emacs tmux xinitrc xresources local-scripts

update: update-emacs update-repo

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


xresources:
	echo TODO

local-scripts:
	if ! [ -d $(HOME)/.local/bin ]; then \
		mkdir -p $(HOME)/.local/bin; \
	fi
	echo "Installing local scripts..."
	cp -p .local/bin/* $(HOME)/.local/bin;
	echo "Done!"

local-desktop-files:
	if ! [ -d $(HOME)/.local/share/applications ]; then \
		mkdir -p $(HOME)/.local/share/applications; \
	fi
	echo "Installing local desktop files..."
	cp -p .local/share/applications/* $(HOME)/.local/share/applications/;
	echo "Done!"

conkeror:
	if [ -e $(HOME)/.conkerorrc ]; then \
		echo "Backuping old .conkerorrc to .conkerorrc.bak"; \
		mv $(HOME)/.conkerorrc $(HOME)/.conkerorrc.bak; \
	fi
	echo "Installing conkeror config..."
	cp -pr .conkerorrc $(HOME)/
	echo "Done!"

zsh:
	if [ -e $(HOME)/.zshrc ]; then \
		echo "Backuping old .zshrc to .zshrc.bak"; \
		mv $(HOME)/.zshrc $(HOME)/.zshrc.bak; \
	fi
	if [ -d $(HOME)/.zsh.d ]; then \
		echo "Backuping old .zsh directory to .zsh.bak"; \
		mv $(HOME)/.zsh $(HOME)/.zsh.bak; \
	fi
	echo "Installing zsh config..."
	cp -pr zsh $(HOME)/.zsh.d
	echo "Done!"

update-emacs:
	echo TODO
