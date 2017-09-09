if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$HOME/git-tf:$PATH:/usr/local/share/npm/bin"
fi

if [[ -s $HOME/.rvm/scripts/rvm ]] ; then source $HOME/.rvm/scripts/rvm ; fi

PS1='\e[0;32m[\u]\e[m$(__git_ps1 "[%s]")\] \e[0;36m\@\e[m\e[0;34m $(pwd)\e[m\n:->'
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
if [ -f ~/.git-completion.bash ]; then
    . ~/.git-completion.bash
fi
export PATH=~/bin:/usr/local/bin:/usr/local/mysql/bin:$PATH
export EDITOR=vim
export PATH=/usr/local/share/python:$PATH
source "$HOME/.aliases"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PATH="$PATH:/usr/local/Cellar/node@6/6.10.3/bin"
export SSL_CERT_FILE=/Users/patrick/.gem/cert.pem

