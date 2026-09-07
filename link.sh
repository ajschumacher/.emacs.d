#!/usr/bin/env bash
# Symlink the dotfiles in this repo into $HOME.  Safe to run more than
# once: links that are already correct are left alone, and anything real
# that would be clobbered is moved aside first.

set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
stamp="$(date +%Y%m%d%H%M%S)"

link() {
  local src="$1" dest="$2"
  if [ ! -e "$src" ]; then
    echo "  ?? missing, skipping: $src"
    return
  fi
  if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$src" ]; then
    echo "  ok $dest"
    return
  fi
  if [ -e "$dest" ] || [ -L "$dest" ]; then
    mv "$dest" "$dest.backup-$stamp"
    echo "  -> moved existing $dest to $dest.backup-$stamp"
  fi
  ln -s "$src" "$dest"
  echo "  + $dest"
}

link "$here/git/.gitconfig" "$HOME/.gitconfig"

link "$here/bash/.bashrc" "$HOME/.bashrc"
# Login shells read .bash_profile, other shells read .bashrc; the
# distinction is rarely worth keeping, so point one at the other.
link "$HOME/.bashrc" "$HOME/.bash_profile"
# Machine-specific settings that should not be in git.
[ -e "$HOME/.bashrc.local" ] || touch "$HOME/.bashrc.local"

# Fallbacks, used only when Homebrew's git has not supplied its own.
link "$here/bash/.git-completion.sh" "$HOME/.git-completion.sh"
link "$here/bash/.git-prompt.sh" "$HOME/.git-prompt.sh"

mkdir -p "$HOME/.ipython/profile_default"
link "$here/ipython/ipython_config.py" \
     "$HOME/.ipython/profile_default/ipython_config.py"
