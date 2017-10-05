# ~/.zshrc
# Original:
# $Id: zshrc 144 2012-04-19 23:01:12Z gnitset $
#source ~/.zsh/config/.zshrc

setopt NO_CLOBBER
setopt NO_PROMPT_CR
setopt NO_BEEP
setopt AUTO_MENU
setopt COMPLETE_IN_WORD
setopt EXTENDED_GLOB
setopt AUTOCD
setopt INTERACTIVE_COMMENTS
setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt SHARE_HISTORY

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*' special-dirs true

bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^r' history-incremental-search-backward
bindkey '^y' yank
bindkey '^t' delete-word
bindkey '^x' copy-prev-shell-word
bindkey '^z' vi-undo-change
bindkey '\e.' insert-last-word

autoload -U compinit
compinit -d ~/.zcompdump.`hostname`
setopt ALL_EXPORT

ZSH_CONFDIR=~/.zsh

###########
# Import separated config files
##########

######
## Plugins
#####
source $ZSH_CONFDIR/plugins/zsh-git-prompt/zshrc.sh
source $ZSH_CONFDIR/lib/npm-modules.zsh

######
## Aliases and other profile related settings
#####
source $ZSH_CONFDIR/lib/aliases.zsh
source $ZSH_CONFDIR/lib/paths.zsh
source $ZSH_CONFDIR/lib/profile.zsh

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

local ret_status="%(?:%{$fg_bold[green]%}=):%{$fg_bold[red]%}=()"
function battery() {
 if $BATTERY ; then;
     echo "$(battery_level_gauge)"
 else
     echo ""
 fi
}

# Default values for the appearance of the prompt. Configure at will.
ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[$P_COLOR]%}%{✚%G%}"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_no_bold[$P_COLOR]%}"

PROMPT='%B%F{$P_COLOR}%}%3~%f %F{$P_COLOR}%D{%H:%M}%f $(git_super_status)%b
%F{$P_COLOR}%}%n%f%F{white} %# %f'

# Hack to ensure the startup path to be the last path opened in urxvt
# Override the cd command with this ..
function chpwd() {
	pwd >! ~/.last_dir
}

# Restore the last saved path
if [ -f ~/.last_dir ]
	then cd $(cat ~/.last_dir)
fi
