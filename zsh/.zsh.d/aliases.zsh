# General aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias se='setxkbmap -model emacs2 -option ctrl:nocaps,compose:rwin se'
alias dv='xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mydvorak $DISPLAY 2> /dev/null'

alias svim='sudo vim'

alias py='python'

alias back='cd -'

# Bluetooth
alias sony-connect='bluetoothctl connect 38:18:4C:D3:1A:20'
alias sony-disconnect='bluetoothctl disconnect 38:18:4C:D3:1A:20'
alias jabra-connect='bluetoothctl connect 70:BF:92:2A:37:DD'
alias jabra-disconnect='bluetoothctl disconnect 70:BF:92:2A:37:DD'

# Bat is cat clone with wings https://github.com/sharkdp/bat
alias cat=bat

# Git aliases
alias gst='git status'
alias gl='git log'
alias gp='git pull'
alias gm='git merge --ff-only'
alias grst='git reset --hard HEAD'
# Mnemonic: Git Rebase On Master
alias grom='git fetch origin && git rebase --autostash origin/master'
alias gsu='git submodule update --init --recursive'

# Systemctl stuff
alias restart='systemctl restart'
alias status='systemctl status'
alias start='systemctl start'

alias please='sudo $(fc -ln -1)'
alias balena-login="pass notes/balena-api-key | xargs sudo balena login -t"

# Emacs aliases
alias emproj='emacs --eval "(setq server-name \"$(basename $PWD)\")" \
      		    --eval "(server-start)"'
