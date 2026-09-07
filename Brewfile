# Brewfile -- everything this config expects to find on a Mac.
# Install with:  brew bundle
# Check with:    brew bundle check

# A bash newer than the one Apple ships (which is stuck at 3.2).
brew "bash"
brew "bash-completion@2"

# Homebrew's git is much newer than Apple's, and brings its own
# completion and prompt scripts (bash/.bashrc prefers those, and falls
# back to the copies vendored in bash/ when they are missing).
brew "git"
# .gitconfig marks the lfs filter as required, so git needs it present.
brew "git-lfs"

# Spell checking, for flyspell.  init.el looks for aspell by name.
brew "aspell"

# consult-ripgrep and projectile both use it when it is there.
brew "ripgrep"

# The language server eglot talks to.  Without it Python still edits
# fine, eglot just doesn't start.
# (pyenv is deliberately not here: it is installed by install.sh via
# pyenv.run into ~/.pyenv, which is the layout .bashrc expects.  A brew
# pyenv would be a second copy, shadowed by ~/.pyenv/bin anyway.)
brew "python-lsp-server"

# Emacs itself, from emacsformacosx.com.
# (Formerly the "emacs" cask, which was renamed.)
cask "emacs-app"
