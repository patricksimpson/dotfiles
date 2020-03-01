# Ignore commands starting with a space, duplicates, and a few others.
export HISTIGNORE="[ ]*:&:bg:fg:ls -l:ls -al:ls -la:ls1:lsa:lsr:gits:gits?"
export HISTCONTROL=ignoreboth:erasedups
export HISTFILESIZE=200000
export HISTSIZE=200000

export PATH="node_modules/.bin:$PATH"

# emacs M-x shell
if [ "dumb" = "$TERM" ] ; then
    alias m='cat'
    alias less='cat'
    alias more='cat'
    export PAGER=cat
    export TERM=xterm-color
else
    alias l='less'
    alias m='more'
fi