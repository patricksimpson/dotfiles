# Install

`git clone git@github.com:patricksimpson/dotfiles.git ~/dotfiles`

## Emacs

`ln -s ~/dotfiles/emacs/.emacs ~/.emacs.el`

### These directories assumed to exist on the local file system:

- Notes: `~/notes`
- Org Mode: `~/org`
- Projects for Projectile: `~/projects`
- Temp (backups, autosaves) files: `~/temp/emacs`

## Bash/ZSH

I use ZSH, I use the default installs with minor adjustments to the zshrc file:

### Themes

    ZSH_THEME="dpoggi"
    
or 

    ZSH_THEME="blinks"

### Plugins

    plugins=(git ruby node npm nvm sudo rbenv postgres)

### Extras 

I have a few aliases (shortcuts) that I've used for years:

    source ~/dotfiles/bash/bashrc.sh
    source ~/dotfiles/bash/aliases.sh
    
## Vim

:skull:

I've retired my vim settings in ~2016. 

Here are my dotfiles, for posterity.

I've used many vim plugins and were an inspiration for many of my current emacs settings...

- Vundle
- nerdtree
- tcomment
- ctrlp
- vim-fugitive
- vim-surround
- vim-git
- ack
- emmet
- airline
- Figlet
- ag
- vimux
