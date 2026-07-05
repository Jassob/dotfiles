#!/usr/bin/env bash
#
# Configures bash to interact with emacs
# supports:
# - vterm
# - M-x shell
# - term
# - shell from outside emacs
#
## Availables functions
#  ALIAS    DESCRIPTION         FROM EXTERNAL TERMINAL      FROM INSIDE EMACS
#  f        file                Open in terminal            Send to server,
#                                                           jumps to it
# sf        sudo file           Open in terminal            Send to server,
#                                                           jumps to it
#

# Emacs client
export ALTERNATE_EDITOR=""
export EMACS_SUDO_PREFIX="/sudo::"

export EMACS_SERVER_NAME=${EMACS_SERVER_NAME:-server}

_emacs_sudo () {
    echo "$@" | xargs -n1 realpath | sed -e "s;^;$EMACS_SUDO_PREFIX;" | xargs >&2
    echo "$@" | xargs -n1 realpath | sed -e "s;^;$EMACS_SUDO_PREFIX;" | xargs
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

_emacs_edit_from_inside () {
    for file in "$@"; do
        # VTERM support
        if [[ -n ${EMACS_VTERM_PATH} ]]; then
            vterm_cmd find-file-other-window "$file"
        else
            emacsclient -s "${EMACS_SERVER_NAME}" -n -e "(find-file-other-window \"$file\")"
        fi
    done
}

_emacs_edit_from_outside () {
    emacsclient -t "$@"
}

_emacs_sudo_edit_from_inside () {
    _emacs_edit_from_inside "$(_emacs_sudo "$@")"
}

_emacs_sudo_edit_from_outside () {
    _emacs_edit_from_outside "$(_emacs_sudo "$@")"
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

if [ -n "$INSIDE_EMACS" ]; then
    export EDITOR=_emacs_edit_from_inside
    alias  f=_emacs_edit_from_inside
    alias sf=_emacs_sudo_edit_from_inside

    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
else
    export EDITOR=_emacs_edit_from_outside
    alias  f=_emacs_edit_from_outside           # open in terminal
    alias sf=_emacs_sudo_edit_from_outside
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
    source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
fi

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
