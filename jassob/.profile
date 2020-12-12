# Email settings
export EMAIL="jacob.t.jonsson@gmail.com"
export NAME="Jacob Jonsson"
export EMAILSERVER="smtp.gmail.com"

# Print using Chalmers printers
export CUPS_GSSSERVICENAME=HTTP

if [ ! "$BROWSER" ]; then
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

[ -d "${HOME}/.local/bin" ] && export PATH="${HOME}/.local/bin:${PATH}"
[ -d "${HOME}/.cabal/bin" ] && export PATH="${HOME}/.cabal/bin:${PATH}"
[ -d "/opt/local/bin" ] && export PATH="/opt/local/bin:${PATH}"
[ -d "/opt/local/bin" ] && export PATH="/opt/local/sbin:${PATH}"

[ -d "/opt/local/man" ] && export MANPATH="/opt/local/man:${MANPATH}"

GPG_TTY=$(tty)
export GPG_TTY

# Add local/bin/ to PATH
export PATH="~/.local/bin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
