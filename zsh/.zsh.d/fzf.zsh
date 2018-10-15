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

