# Email settings
export EMAIL="jacob.t.jonsson@gmail.com"
export NAME="Jacob Jonsson"
export EMAILSERVER="smtp.gmail.com"

# Print using Chalmers printers
export CUPS_GSSSERVICENAME=HTTP

if [ ! $BROWSER ]; then
    export BROWSER="google-chrome"
fi

# Locale
export LC_CTYPE="sv_SE.UTF-8"
export LC_NUMERIC="sv_SE.UTF-8"
export LC_TIME="sv_SE.UTF-8"
export LC_MONETARY="sv_SE.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_PAPER="sv_SE.UTF-8"
export LC_NAME="en_US.UTF-8"
export LC_ADDRESS="en_US.UTF-8"
export LC_TELEPHONE="en_US.UTF-8"
export LC_MEASUREMENT="sv_SE.UTF-8"
export LC_IDENTIFICATION="en_US.UTF-8"

# User installed programs
if [ -d ~/bin ] ; then
    export PATH="${HOME}/bin:${PATH}"
fi
if [ -d ~/sbin ] ; then
    export PATH="${HOME}/sbin:${PATH}"
fi
if [ -d ~/man ] ; then
    export MANPATH="${HOME}/man:${MANPATH}"
fi
if [ -d ~/share/man ] ; then
    export MANPATH="${HOME}/share/man:${MANPATH}"
fi
if [ -d ~/.local/bin ] ; then
    export PATH="${HOME}/.local/bin:${PATH}"
fi
if [ -d ~/.cabal/bin ] ; then
    export PATH="${HOME}/.cabal/bin:${PATH}"
fi

# Installed programs
if [ -d /opt/local/bin ] ; then
    export PATH="/opt/local/bin:${PATH}"
    if [ -d /opt/local/sbin ] ; then
	export PATH="/opt/local/sbin:${PATH}"
    fi
    if [ -d /opt/local/man ] ; then
	export MANPATH="/opt/local/man:${MANPATH}"
    fi
fi

# Add local/bin/ to PATH
export PATH="~/.local/bin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
