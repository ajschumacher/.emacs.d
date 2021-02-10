#!/usr/bin/env bash

ln -s ~/.emacs.d/git/.gitconfig ~/.gitconfig
ln -s ~/.emacs.d/bash/.bashrc ~/.bashrc
touch ~/.bashrc.local
ln -s ~/.bashrc ~/.bash_profile
ln -s ~/.emacs.d/bash/.git-completion.sh ~/.git-completion.sh
ln -s ~/.emacs.d/bash/.git-prompt.sh ~/.git-prompt.sh
ln -s ~/.emacs.d/ruby/.irbrc ~/.irbrc
ln -s ~/.emacs.d/haskell/.ghci ~/.ghci
ln -s ~/.emacs.d/vim/.vimrc ~/.vimrc
mkdir -p ~/.ipython/profile_default
ln -s ~/.emacs.d/ipython/ipython_config.py ~/.ipython/profile_default/ipython_config.py
