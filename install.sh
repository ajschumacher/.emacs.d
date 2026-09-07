#!/usr/bin/env bash
# Set this machine up.  Safe to run more than once.

set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

say() { printf '\n==> %s\n' "$*"; }

# --- Homebrew ---------------------------------------------------------
if ! command -v brew >/dev/null 2>&1; then
  say "Installing Homebrew"
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  # Apple silicon puts it in /opt/homebrew; Intel in /usr/local.
  for candidate in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    [ -x "$candidate" ] && eval "$("$candidate" shellenv)"
  done
fi

say "Installing everything in the Brewfile"
brew bundle --file="$here/Brewfile"

# --- Dotfile symlinks -------------------------------------------------
say "Linking dotfiles"
"$here/link.sh"

# --- pyenv ------------------------------------------------------------
# Installed into ~/.pyenv, which is what bash/.bashrc sets PYENV_ROOT to.
if [ ! -d "$HOME/.pyenv" ]; then
  say "Installing pyenv"
  curl -fsSL https://pyenv.run | bash
else
  say "pyenv already present at ~/.pyenv"
fi

# --- Emacs packages ---------------------------------------------------
# init.el installs what is missing on its own at first launch, but doing
# it here means the first real launch is not a two-minute wait.
say "Installing Emacs packages"
emacs --batch -l "$here/init.el" \
      --eval '(package-install-selected-packages t)' 2>&1 |
  grep -viE 'lexical-binding|You can add one|for more information|^$' || true

# --- Tree-sitter grammars ---------------------------------------------
# python-ts-mode is only used when its grammar is actually built; init.el
# checks, so this failing is not fatal.
say "Installing the tree-sitter Python grammar"
emacs --batch -l "$here/init.el" \
      --eval '(treesit-install-language-grammar (quote python))' >/dev/null 2>&1 &&
  echo "  ok" ||
  echo "  skipped (needs git and a C compiler; python-mode still works)"

say "Done."
cat <<'NOTE'

Still to do by hand:
  * Make Homebrew's bash the login shell:
      echo "$(brew --prefix)/bin/bash" | sudo tee -a /etc/shells
      chsh -s "$(brew --prefix)/bin/bash"
  * Generate an ssh key and add it to GitHub:
      ssh-keygen -t ed25519 -C "ajschumacher@gmail.com"
  * The System Settings tweaks listed in README.md.
NOTE
