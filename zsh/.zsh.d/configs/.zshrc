# ~/.zshrc

# Source FILE if it exists
function sourceIfExists {
    FILE=${1:-undefined}

    if [[ "$FILE" == undefined ]]; then return 0; fi

    if [[ -f "$FILE" ]]; then
	source "$FILE"
    fi
}

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

# Disable C-s to stop the terminal accepting input
stty -ixon

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*' special-dirs true

# Use emacs keymap
bindkey -e

ZDOTDIR=~/.zsh.d

autoload -U compinit
compinit -d
setopt ALL_EXPORT

######
## Sourcing other files
#####
sourceIfExists "$ZDOTDIR/aliases.zsh"

if [[ -f /etc/NIXOS ]]; then
    sourceIfExists "$ZDOTDIR/nix.zsh"
fi

######
## Theme
#####
PROMPT_COLORS="yellow
blue
green
cyan
yellow
magenta"

P_COLOR=$(echo $PROMPT_COLORS | sort -R | tail -n 1)

PROMPT="%B%F{$P_COLOR}%}%3~%f %F{$P_COLOR}%D{%H:%M}%f%b
%F{$P_COLOR}%}%n%f%F{white} %# %f"

# Hack to ensure the startup path to be the last path opened in terminal
# Override the cd command with this ..
function chpwd() {
	pwd >! ~/.cache/last_dir
}

# Restore the last saved path
if [ -f ~/.last_dir ]
	then cd "$(cat ~/.cache/last_dir)"
fi

# Initialize fasd
eval "$(fasd --init auto)"

# Initialize fzf
[ -f ~/.zsh.d/fzf.zsh ] && source ~/.zsh.d/fzf.zsh
