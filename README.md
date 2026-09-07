# My config files

Emacs configuration and assorted dotfiles, living in `~/.emacs.d` and
symlinked out from there.


## New machine

```shell
# Sign in to iCloud, install Chrome and whatever else, then:
xcode-select --install
git clone https://github.com/ajschumacher/.emacs.d.git ~/.emacs.d
~/.emacs.d/install.sh
```

`install.sh` installs Homebrew if it is missing, runs `brew bundle` over
the `Brewfile`, links the dotfiles, and installs the Emacs packages. It
is safe to run again later to pick up new dependencies.

Three things it deliberately leaves to you, because they need a password
or a browser:

```shell
# Use Homebrew's bash instead of the ancient one Apple ships
echo "$(brew --prefix)/bin/bash" | sudo tee -a /etc/shells
chsh -s "$(brew --prefix)/bin/bash"

# An ssh key, to be pasted into GitHub afterwards
ssh-keygen -t ed25519 -C "ajschumacher@gmail.com"
```

Then switch the clone to ssh, so pushing works:

```shell
git -C ~/.emacs.d remote set-url origin git@github.com:ajschumacher/.emacs.d.git
```

Add to `~/.ssh/config`:

```text
Host *
  IgnoreUnknown UseKeychain
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_ed25519
```


## What is in here

| Path | What it is |
| --- | --- |
| `early-init.el` | Frame chrome and GC, before the first frame is drawn |
| `init.el` | The Emacs configuration, and the list of packages it needs |
| `elisp/` | Vendored elisp that no package archive carries any more |
| `snippets/` | yasnippet snippets |
| `abbrev_defs` | Abbreviations |
| `bash/` | `.bashrc`, plus fallback git completion and prompt scripts |
| `git/` | `.gitconfig` |
| `ipython/` | IPython profile |
| `Brewfile` | Everything to install with `brew bundle` |
| `install.sh` | Set up a machine |
| `link.sh` | Symlink the dotfiles into `$HOME` |

Emacs packages are **not** committed. `init.el` holds the list in
`package-selected-packages`, and they are installed from MELPA and GNU
ELPA on first launch.


## Mac tweaks

 * `System Settings`, `Keyboard`, `Keyboard Shortcuts...`, `Modifier
   Keys...`, `Caps Lock` to `Control`
 * Same place, `Screenshots`: disable `Screenshot and recording
   options` (low value, and conflicts with a useful Emacs key
   combination)
 * `Finder`, `Settings`, `Advanced`, `Show all filename extensions`
 * Terminal settings
     * `Profiles` - `Keyboard` - `Use Option as Meta key` on
     * `Profiles` - `Advanced` - `Audible bell` off
     * `Profiles` - `Advanced` - `Visual bell` - `Only when sound is muted` off
     * `Profiles` - `Text` - Menlo Regular 18 pt.

```shell
# turn off window drop shadows when doing screenshots
defaults write com.apple.screencapture disable-shadow -bool true; killall SystemUIServer

# make all files visible in the Finder
defaults write com.apple.finder AppleShowAllFiles TRUE; killall Finder

# convince the hostname to be what you want
scutil --set ComputerName "name"
scutil --set LocalHostName "name"
scutil --set HostName "name"
```


## Other things

 * Custom local short names for IP addresses go in `/etc/hosts`.
 * `~/.bashrc.local` is sourced by `.bashrc` and is not in git, so it is
   the place for anything machine-specific or secret.
 * Emacs writes its Custom settings to `custom.el`, which is not in git.
