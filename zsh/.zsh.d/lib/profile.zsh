# Email settings
export EMAIL="jacob.t.jonsson@gmail.com"
export NAME="Jacob Jonsson"
export EMAILSERVER="smtp.gmail.com"

# Print using Chalmers printers
export CUPS_GSSSERVICENAME=HTTP

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
   EDITOR='emacsclient -a "" -c'
else
   EDITOR='emacs'
fi

if [[ ! $BROWSER ]]; then
	BROWSER="firefox"
fi

# Shell history
HISTSIZE=4096
HISTFILE=$ZDOTDIR/history
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
