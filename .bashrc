function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

export PS1="★ \[\033[01;35m\]\$(parse_git_branch)\[\033[01;34m\] \[\033[01;35m\]\w \[\033[01;34m\] $\[\e[0m\] "

alias l='ls'
alias sl='ls'
alias e='emacs'
alias enw='emacs -nw'
alias g='git'

export PREFIX=$HOME/usr
export PKG_CONFIG_PATH=${PREFIX}/lib/pkgconfig
export VARNISHSRC=$HOME/git/varnish-cache
export MANPATH=$HOME/usr/share/man

function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
export PS1="★ \[\033[01;32m\]\$(parse_git_branch)\[\033[01;34m\] \[\033[01;35m\]\w \[\033[01;34m\] $\[\e[0m\] "

export PATH=$HOME/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/venv/bin:$PATH


export LESS='-R'
export LESSOPEN='| pygmentize "%s"'

alias clean='find . -type f -name "*~" -exec rm {} \;'
