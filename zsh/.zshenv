# Ubuntu calls compinit in /etc/zsh/zshrc, unless
# $skip_global_compinit is set.
#
# This causes issues if you want to reuse the completion cache instead
# of recreating it every time. By turning off global compinit we get
# wanted (and expected) behavior.
skip_global_compinit=1

# Ubuntu does not include /usr/share/zsh/site-functions in fpath (but
# it does include /usr/local/share/zsh/site-functions).
fpath=("/usr/share/zsh/site-functions" $fpath)

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# add /usr/local/go/bin to PATH
if [ -d "/usr/local/go/bin" ] ; then
    PATH="/usr/local/go/bin:$PATH"
fi

# add ~/.cargo/bin to PATH
if [ -d "${HOME}/.cargo/bin" ] ; then
    PATH="${HOME}/.cargo/bin:${PATH}"
fi

# add ~/go/bin to PATH
if [ -d "${HOME}/go/bin" ] ; then
    PATH="${HOME}/go/bin:${PATH}"
fi

# allow gpg-agent to manage SSH keys
export GPG_TTY=/dev/pts/3
export SSH_AUTH_SOCK=/run/user/1000/gnupg/S.gpg-agent.ssh

# set script directory root
export SD_ROOT="/home/jassob/scripts"

export EDITOR="emacsclient -nw"
