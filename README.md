# Jassob's dotfiles
This is a collection of my configuration files.

Some of my configurations exists as other repos and are in that case
added as submodules. My dotfiles are organized so that [GNU
Stow](https://www.gnu.org/software/stow/) can be used, with [this blog
post](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html)
as reference.

## Setup
1. Clone this repo into $HOME/dotfiles

   $ git clone --recursive https://github.com/Jassob/dotfiles ~/dotfiles

2. Use GNU stow to install the configs, for instance with emacs

	$ stow emacs

3. Profit!
