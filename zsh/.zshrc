# ~/.zshrc
typeset -U path cdpath fpath manpath

setopt NO_CLOBBER
setopt NO_PROMPT_CR
setopt NO_BEEP
setopt AUTO_MENU
setopt COMPLETE_IN_WORD
setopt EXTENDED_GLOB
setopt INTERACTIVE_COMMENTS
setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt SHARE_HISTORY
setopt ALL_EXPORT

export HISTSIZE=4096
export HISTFILE=$HOME/.shell/history
export SAVEHIST=3072

# Disable C-s to stop the terminal accepting input
stty -ixon

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*' special-dirs true

# Use emacs keymap
bindkey -e
bindkey '^[[1;5C' emacs-forward-word
bindkey '^[[1;5D' emacs-backward-word

# Load plugins
eval "$(sheldon source)"
eval "$(starship init zsh)"

# Activate auto-suggestions
source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# Activate syntax highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source ~/.shell/work.sh
source ~/.shell/emacs.sh

# Initialize fasd
# eval "$(fasd --init auto)"

## Aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias se='setxkbmap -model emacs2 -option ctrl:nocaps,compose:rwin se'
alias back='cd -'
alias please='sudo $(fc -ln -1)'
alias s="sd"
alias e="emacsclient -nw -s server"

autoload -Uz compinit
# Only regenerate completion cache once every day
if [ ! -f ~/.zcompdump ]; then
    compinit
elif (( $(date +'%s') - $(stat -c '%Y' ~/.zcompdump) < 3600 * 24 )); then
    # file exists and is younger than a day, use cache
    compinit -C
else
    compinit
fi

# JJ completions
eval "$(jj util completion zsh)"
