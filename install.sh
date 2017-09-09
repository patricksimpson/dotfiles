#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
olddir=~/dotfiles_old             # old dotfiles backup directory

bash_dir=$dir/bash
zsh_dir=$dir/zsh
vim_dir=$dir/vim
emacs_dir=$dir/emacs

bash_files="aliases bash_profile bashrc functions profile"
zsh_files="zprofile zshrc"
vim_files="vimrc viminfo"
files="gitconfig irbrc tmux.conf vim/autocmd.vim vim/bundle.vim vim/functions.vim vim/mappings.vim vim/settings.vim" # list of files/folders to symlink in homedir

##########

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir

# change to the dotfiles directory
echo "Changing to the $dir directory"
cd $dir

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks 
for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv ~/.$file $olddir
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/.$file
done

for file in $bash_files; do
    echo "Moving any existing BASH dotfiles from ~ to $olddir"
    mv ~/.$file $olddir
    echo "Creating symlink to $file in home directory."
    ln -s $bash_dir/$file ~/.$file
done

for file in $zsh_files; do
    echo "Moving any existing ZSH dotfiles from ~ to $olddir"
    mv ~/.$file $olddir
    echo "Creating symlink to $file in home directory."
    ln -s $zsh_dir/$file ~/.$file
done

for file in $vim_files; do
    echo "Moving any existing VIM dotfiles from ~ to $olddir"
    mv ~/.$file $olddir
    echo "Creating symlink to $file in home directory."
    ln -s $vim_dir/$file ~/.$file
    ln -s $vim_dir ~/.vim
done
