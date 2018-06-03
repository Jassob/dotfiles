# Email settings
export EMAIL="jacob.t.jonsson@gmail.com"
export NAME="Jacob Jonsson"
export EMAILSERVER="smtp.gmail.com"

# Print using Chalmers printers
export CUPS_GSSSERVICENAME=HTTP

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
LC_MONETARY="sv_SE.UTF-8"
LC_MESSAGES="en_US.UTF-8"
LC_PAPER="sv_SE.UTF-8"
LC_NAME="en_US.UTF-8"
LC_ADDRESS="en_US.UTF-8"
LC_TELEPHONE="en_US.UTF-8"
LC_MEASUREMENT="sv_SE.UTF-8"
LC_IDENTIFICATION="en_US.UTF-8"

# User installed programs
if [[ -d ~/bin ]] ; then
    PATH="${HOME}/bin:${PATH}"
fi
if [[ -d ~/sbin ]] ; then
    PATH="${HOME}/sbin:${PATH}"
fi
if [[ -d ~/man ]] ; then
    MANPATH="${HOME}/man:${MANPATH}"
fi
if [[ -d ~/share/man ]] ; then
    MANPATH="${HOME}/share/man:${MANPATH}"
fi
if [[ -d ~/.local/bin ]] ; then
	PATH="${HOME}/.local/bin:${PATH}"
fi
if [[ -d ~/.cabal/bin ]] ; then
    PATH="${HOME}/.cabal/bin:${PATH}"
fi

# Installed programs
if [[ -d /opt/local/bin ]] ; then
    PATH="/opt/local/bin:${PATH}"
    if [[ -d /opt/local/sbin ]] ; then
	PATH="/opt/local/sbin:${PATH}"
    fi
    if [[ -d /opt/local/man ]] ; then
	MANPATH="/opt/local/man:${MANPATH}"
    fi
fi

unsetopt ALL_EXPORT

unset MAILCHECK
unset ROOTPATH
