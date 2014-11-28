prompt_char() {
    git branch >/dev/null 2>/dev/null && echo '±' && return
    hg root >/dev/null 2>/dev/null && echo '☿' && return
    echo '○'
}

setopt PROMPT_SUBST

## Customize prompt
if [ "$EUID" = "0" ] || [ "$USER" = "root" ] ; then
		PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:${ROOTPATH}"
		PROMPT=$'%{\e[36;1m%}%n %{\e[m%}at %{\e[34;1m%}%5m %{\e[m%}in %{\e[32;0m%}%~%{\e[m%} $(git_super_status)\n$(prompt_char) %{\e[34;1m%}%T%{\e[m%} %{\e[36m%}%# %{\e[m%}'
else
		PATH="/usr/local/bin:/usr/bin:/bin:${PATH}"

                PROMPT=$'%{\e[36;1m%}%n %{\e[m%}at %{\e[34;1m%}%5m %{\e[m%}in %{\e[32;1m%}%~%{\e[m%} $(git_super_status)\n$(prompt_char) %{\e[34;1m%}%T%{\e[m%} %{\e[36m%}%# %{\e[m%}'
fi

## Right prompt
RPROMPT=$'%{\e[34;1m%}%D{%a %d %b %G}%{\e[m%} %0(?:%{\e[36;1m%}:%{\e[31m%})[%?]%{\e[m%}'
