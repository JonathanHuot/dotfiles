function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

export PS1="★ \[\033[01;35m\]\$(parse_git_branch)\[\033[01;34m\] \[\033[01;35m\]\w \[\033[01;34m\] $\[\e[0m\] "

alias l='ls'
alias sl='ls'
alias e='emacs'
alias enw='emacs -nw'
