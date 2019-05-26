# Setup fzf
# ---------
if [[ -e ~/.fzf ]]; then
    # Setup fzf
    # ---------
    if [[ ! "$PATH" == */home/jassob/.fzf/bin* ]]; then
	export PATH="$PATH:/home/jassob/.fzf/bin"
    fi

    # Auto-completion
    # ---------------
    [[ $- == *i* ]] && source "/home/jassob/.fzf/shell/completion.zsh" 2> /dev/null

    # Key bindings
    # ------------
    source "/home/jassob/.fzf/shell/key-bindings.zsh"
else
    # Find Nix path to FZF
    FZF_PATH=$(whereis fzf | xargs readlink | xargs dirname | xargs dirname)

    # Auto-completion
    # ---------------
    [[ $- == *i* ]] && source "$FZF_PATH/share/fzf/completion.zsh" 2> /dev/null

    # Key bindings
    # ------------
    source "$FZF_PATH/share/fzf/key-bindings.zsh"
fi
