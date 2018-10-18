# Setup fzf
# ---------
FZF_PATH=$(whereis fzf | awk '{ print $2 }' | xargs dirname | xargs dirname)

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "$FZF_PATH/share/fzf/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "$FZF_PATH/share/fzf/key-bindings.zsh"
