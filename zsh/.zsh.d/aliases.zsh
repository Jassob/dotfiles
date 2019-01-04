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
alias plattan-connect='bluetoothctl connect 5C:EB:68:45:78:6D'
alias plattan-disconnect='bluetoothctl disconnect 5C:EB:68:45:78:6D'

# Git aliases
alias gst='git status'
alias gl='git log'
alias gp='git pull'
alias gm='git merge --ff-only'
alias grst='git reset --hard HEAD'
# Mnemonic: Git Rebase On Master
alias grom='git fetch origin && git rebase origin/master'
alias gsu='git submodule update --init --recursive'

# Systemctl stuff
alias restart='systemctl restart'
alias status='systemctl status'
alias start='systemctl start'

alias please='sudo $(fc -ln -1)'
alias balena-login="pass notes/balena-api-key | xargs sudo balena login -t"
