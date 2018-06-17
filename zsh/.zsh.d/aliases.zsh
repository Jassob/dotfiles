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

# Git aliases
alias gst='git status'
alias gl='git log'
alias gp='git pull'

# Systemctl stuff
alias restart='systemctl restart'
alias status='systemctl status'
alias start='systemctl start'

alias please='sudo $(fc -ln -1)'
