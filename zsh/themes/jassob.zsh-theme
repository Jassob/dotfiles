setopt prompt_subst

local ret_status="%(?:%{$fg_bold[green]%}=):%{$fg_bold[red]%}=()"
function battery() {
 if $BATTERY ; then;
     echo "$(battery_level_gauge)"
 else
     echo ""
 fi
}

# Default values for the appearance of the prompt. Configure at will.
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[black]%}(${reset_color}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg_bold[black]%})"
ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[green]%}"
ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[red]%}%{●%G%}"
ZSH_THEME_GIT_PROMPT_CONFLICTS="%{$fg[red]%}%{✖%G%}"
ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[blue]%}%{✚%G%}"
ZSH_THEME_GIT_PROMPT_BEHIND="%{↓%G%}"
ZSH_THEME_GIT_PROMPT_AHEAD="%{↑%G%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg_bold[red]%}…"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg_bold[green]%}%{✔%G%}"

PROMPT='%B%F{black}(%f%F{green}%}%3~%f%F{black})%f%F{black}(%f%F{white}%D{%H:%M}%f%F{black})%f$(git_super_status)%b
%F{cyan}%}%n%f%F{white} > %f'
RPROMPT='%F{white}$(battery)%f'
# RPROMPT='$(battery)'