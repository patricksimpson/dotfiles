# Install

`git clone git@github.com:patricksimpson/dotfiles.git ~/dotfiles`

## Emacs

`ln -s ~/dotfiles/emacs/.emacs ~/.emacs`

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

I've retired my vim settings. Made the switch to Emacs in ~2016. 

I've left my dotfiles for posterity.

I used many vim plugins such as:

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
