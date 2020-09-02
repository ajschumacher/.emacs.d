# .bash_profile / .bashrc

# Really .bash_profile is for login shells
# and .bashrc is for non-login shells,
# but the distinction is not always clear
# and it's easy to just have them be the same.


# I was thinking of adding a function for making a directory and then
# changing into it, but I think it's probably just as good to use:
#  mkdir dirname && cd $_
# Especially if other people are going to use my commands.


# Set environment variables for all shells:

export GOPATH=~/.go                # for installing Go things
export PATH=$PATH:$GOPATH/bin      # (like gof3r)
export PATH=$PATH:~/.local/bin     # local installs, largely Python
export PATH=$PATH:~/.cask/bin      # for Cask (Emacs packages)
# Put first so that brew-installed things are found first
export PATH=/usr/local/bin:$PATH   # really ought to be there already
export PATH=/usr/local/sbin:$PATH  # suggested by brew
export PATH=~/.rvm/bin:$PATH       # in case I'm using RVM

# GPU IDs should match up
export CUDA_DEVICE_ORDER=PCI_BUS_ID
# Alternative is FASTEST_FIRST, see:
# https://www.gpugrid.net/forum_thread.php?id=3977

# WORKON_HOME considered harmful;
# default of ~/.local/share/virtualenvs preferred -
# but it's needed for elpy/pyvenv virtualenv support.
export WORKON_HOME=~/.local/share/virtualenvs
# It would be nicer to have pyvenv default like pew, but this will do.
# Note: Anaconda can be used via pew as `pew workon ~/anaconda`.


# If not running interactively, don't do anything.
case $- in         # "$-" expands to current option flags.
    *i*) ;;        # Including "i" means "interactive".
      *) return;;  # If not interactive, don't continue.
esac


# More things for interactive shells:

# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it.
shopt -s histappend

# Control history size.
HISTSIZE=1000      # number of commands remembered in the command history
HISTFILESIZE=2000  # maximum lines stored in the history file

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Make less more friendly for non-text input files. (See lesspipe(1).)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Set up git prompt.
source ~/.git-completion.sh
source ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"

# Grab Python virtual env info for prompt.
if [ -z ${VIRTUAL_ENV+x} ]
then
    VENV_NOTICE=""
else
    VENV_NOTICE=" (py: $(basename "$VIRTUAL_ENV"))"
fi

# Display an informative, nicely spaced prompt.
# Let's use colors! This is not DRY, but it works...
PSred='\n\[\033[0;31m\]\u@\h \w$VENV_NOTICE$(__git_ps1 " (%s)") \d \t\[\033[00m\]\n\$ '
PSgreen='\n\[\033[0;32m\]\u@\h \w$VENV_NOTICE$(__git_ps1 " (%s)") \d \t\[\033[00m\]\n\$ '
PSyellow='\n\[\033[0;33m\]\u@\h \w$VENV_NOTICE$(__git_ps1 " (%s)") \d \t\[\033[00m\]\n\$ '
PSblue='\n\[\033[0;34m\]\u@\h \w$VENV_NOTICE$(__git_ps1 " (%s)") \d \t\[\033[00m\]\n\$ '
PSpink='\n\[\033[0;35m\]\u@\h \w$VENV_NOTICE$(__git_ps1 " (%s)") \d \t\[\033[00m\]\n\$ '
PScyan='\n\[\033[0;36m\]\u@\h \w$VENV_NOTICE$(__git_ps1 " (%s)") \d \t\[\033[00m\]\n\$ '
PS1=$PSblue

# Enable color support by default.
alias ls='ls -Gp'  # this is for a Mac
ls --color=auto &> /dev/null && alias ls='ls --color=auto -p'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Provide short form ls aliases.
alias ll='ls -alF'
alias lh='ls -alFh'
alias la='ls -A'
alias l='ls -1F'

# type less for tmux
alias tl='tmux list-sessions'
alias tn='tmux new-session -s'
alias ta='tmux attach -t'
alias td='tmux detach'

# easily check for a running program
alias got='ps aux | grep'

# easily check for python packages
alias pipi='pip freeze | grep'

# parameters for rsync over ssh
alias srsync='rsync -avz -e ssh --progress'

# (experimental) command-line setdiff
function setdiff () { comm -23 <(sort "$1") <(sort "$2"); }

# Easily start a simple local Python web server.
alias web='python -m SimpleHTTPServer'

# Easily run git.
alias g='git'
alias gi='git'

# Easily run pipenv
alias p='pipenv'

# Easily get IPython Notebook started up.
alias nb='jupyter notebook'

# Emacs in client/server, text or graphical mode:
alias e='emacsclient -c --alternate-editor="" -nw'
alias eg='emacsclient -c --alternate-editor=""'
export EDITOR='emacsclient -c --alternate-editor="" -nw'

# Convenience
alias ..='cd ..'

# protoc via grpc_tools (pip install grpcio-tools)
alias protoc='python -m grpc_tools.protoc'

# load bash-completion 2 on a Mac with brew
if hash brew 2>/dev/null; then
    if [ -f $(brew --prefix)/share/bash-completion/bash_completion ]; then
        . $(brew --prefix)/share/bash-completion/bash_completion
    fi
fi

# get pew virtualenv auto-complete
if hash pew 2>/dev/null; then
    source "$( dirname $(pew shell_config) )"/complete.bash
fi

# easily average a column of numbers
alias average="awk '{sum+=\$1} END {print sum/NR}'"

# Use the following line to make aliasing work in scripts:
# shopt -s expand_aliases

# optional local config
source ~/.bashrc.local
