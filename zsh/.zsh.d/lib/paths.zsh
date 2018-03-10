umask 022

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
