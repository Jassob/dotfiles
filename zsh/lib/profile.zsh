
# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
   # I run a emacs --daemon on login.
   EDITOR='emacsclient -c'
else
   EDITOR='emacs'
fi

# Shell history
HISTSIZE=4096
HISTFILE=~/.zsh/history
SAVEHIST=3072

# Locale
LC_CTYPE="sv_SE.UTF-8"
LC_NUMERIC="sv_SE.UTF-8"
LC_TIME="sv_SE.UTF-8"
# . ignoreras vid sortering av tex lsoutput
#LC_COLLATE="en_US.UTF-8"
LC_MONETARY="sv_SE.UTF-8"
LC_MESSAGES="en_US.UTF-8"
LC_PAPER="sv_SE.UTF-8"
LC_NAME="en_US.UTF-8"
LC_ADDRESS="en_US.UTF-8"
LC_TELEPHONE="en_US.UTF-8"
LC_MEASUREMENT="sv_SE.UTF-8"
LC_IDENTIFICATION="en_US.UTF-8"

unsetopt ALL_EXPORT

unset MAILCHECK
unset ROOTPATH
