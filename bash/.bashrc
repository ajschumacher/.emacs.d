# .bashrc / .bash_profile

# PATH things:
export PATH=/usr/local/bin:$PATH
export PATH=~/anaconda/bin:$PATH
export PATH=~/.cask/bin:$PATH
export PATH=$PATH:$HOME/.rvm/bin

# For virtualenvwrapper:
export WORKON_HOME=~/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh # possibly brittle


# These bits I just copied in from somewhere

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# End bits I just copied in from somewhere


# imports and config for git prompt
source ~/.git-completion.sh
source ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"

PS1='\n\[\033[0;34m\]\u@\h \w$(__git_ps1 " (%s)") \d \t\[\033[00m\]\n\$ '

# enable color support of ls and also add handy aliases
alias ls='ls -Gp'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ll='ls -alF'
alias lh='ls -alFh'
alias la='ls -A'
alias l='ls -CF'

# easily check for a running program
alias got='ps awx | grep'

# run web server quickly
alias web='python -m SimpleHTTPServer'

# lazy git
alias g='git'
alias gi='git'

# all the namespaces
alias nb='ipython notebook --pylab=inline'

# emacs client/server, text or graphical mode
alias e='emacsclient -c --alternate-editor="" -nw'
alias eg='emacsclient -c --alternate-editor=""'

export EDITOR='emacsclient -c --alternate-editor="" -nw'
