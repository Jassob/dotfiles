# Email settings
export EMAIL="jacob.t.jonsson@gmail.com"
export NAME="Jacob Jonsson"
export EMAILSERVER="smtp.gmail.com"

source $HOME/.Xresources.d/colors

# GST paths
export GST_OMX_CONFIG_DIR=/usr/local/etc/xdg/
export GST_PLUGIN_PATH=/usr/local/lib/gstreamer-1.0/
export LD_LIBRARY_PATH=/usr/local/lib/

# Panel settings for panel in bspwm
PANEL_FIFO=/tmp/panel-fifo
PANEL_HEIGHT=32
export PANEL_FIFO PANEL_HEIGHT

# Print using Chalmers printers
export CUPS_GSSSERVICENAME=HTTP

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
   # I run a emacs --daemon on login.
   EDITOR='emacsclient -c'
else
   EDITOR='emacs'
fi

# Shell history
HISTSIZE=4096
HISTFILE=~/.zsh/history
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
