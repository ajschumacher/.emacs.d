# My config files


### Setup

Major GUI apps (Chrome, etc.) just install as needed...

```shell
# Homebrew (https://brew.sh/)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# Check whether it used /opt/homebrew/bin/ or /usr/local/bin/...

# Change shell to latest bash
brew install bash
sudo -s
echo /opt/homebrew/bin/bash >> /etc/shells  # OR: echo /usr/local/bin/bash >> /etc/shells
chsh -s /opt/homebrew/bin/bash  # OR: chsh -s /usr/local/bin/bash
exit
chsh -s /opt/homebrew/bin/bash  # OR: chsh -s /usr/local/bin/bash
# Install appropriate bash-completion
brew install bash-completion@2

# Emacs!
brew install --cask emacs
brew install aspell
brew install ispell  # not sure I need both, but...
# System Preferences - Security & Privacy - Full Disk Access:
#  add /usr/bin/ruby (Shift+Command+Period to see /usr)
#  maybe grant for Emacs too?

# pyenv (https://github.com/pyenv/pyenv)
# via pyenv-installer (https://github.com/pyenv/pyenv-installer)
curl https://pyenv.run | bash

# set up some Python
pyenv install 3.8.11  # adjust version as desired
# set new Python as default
pyenv global 3.8.11  # adjust version as desired
# Make a venv the pyenv way:
pyenv virtualenv py38a
pyenv activate py38a
# Turn it off again:
pyenv deactivate

# set up an ssh keys
# (https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
ssh-keygen -t ed25519 -C "your_email@example.com"
eval "$(ssh-agent -s)"
```

Add to `~/.ssh/config`:

```text
Host *
  IgnoreUnknown UseKeychain
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_ed25519
```

Continue at command-line:

```shell
ssh-add -K ~/.ssh/id_ed25519
# And don't forget to add in GitHub interface...

# Pull in all my custom settings:
git clone git@github.com:ajschumacher/.emacs.d.git
# Run the `link.sh` script as needed to connect things.
```


### Other Mac tweaks

 * `System Preferences...`, `Keyboard`, `Modifier Keys...`, `Caps Lock
   to Control`
 * `System Preferences...`, `Keyboard`, `Shortcuts...`, Disable
   `Screenshot and recording options` (low value and conflicts with
   useful Emacs key combination)
 * `Finder`, `Preferences`, `Advanced`, `Show all filename extensions`
 * Make Terminal settings reasonable.
     * `Profiles` - `Keyboard` - `Use Option as Meta key` on
     * `Profiles` - `Advanced` - `Audible bell` off
     * `Profiles` - `Advanced` - `Visual bell` - `Only when sound is muted` off
     * `Profiles` - `Text` - Menlo Regular 18 pt.

[in Finder preferences]: http://www.idownloadblog.com/2014/10/29/how-to-show-or-hide-filename-extensions-in-os-x-yosemite/

```
# turn off window drop shadows when doing screenshots
defaults write com.apple.screencapture disable-shadow -bool true; killall SystemUIServer

# make all files visible in the Finder
defaults write com.apple.finder AppleShowAllFiles TRUE; killall Finder

# convince the hostname to be what you want
scutil --set ComputerName "name"
scutil --set LocalHostName "name"
scutil --set HostName "name"
# https://www.techradar.com/how-to/software/operating-systems/how-to-change-your-mac-s-name-using-the-os-x-terminal-1298974
```


### Other things

 * Recall that custom local short names for IP addresses are
   configured in `/etc/hosts`.
