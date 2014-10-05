# My config files

### Setup

 * Install `git`
 * In home directory, `git clone [clone URL]`
 * Run the `link.sh` script as needed to connect things.
 * Install `emacs` 24.
 * Install `[cask][]` for `emacs` dependencies (use python installer!)
 * Install `emacs` package dependencies with `cask install` from
   `~/.emacs.d`
 * Install `aspell` to have spell checking in emacs (and everywhere)
 * `python` and `R` kind of go without saying...
 * `pip install elpy rope` for all the emacs-python love from elpy
   (`jedi` can be used in place of `rope`)
 * possibly install `gnutls` (`brew install gnutls` on mac)

[cask]: https://github.com/cask/cask


### Other Mac tweaks

 * `System Preferences...`, `Keyboard`, `Modifier Keys...`, `Caps Lock to
   Control`
 * The nice default-provided font in Terminal is called "Menlo".
 * To turn off window drop shadows when doing screenshots, run:
   `defaults write com.apple.screencapture disable-shadow -bool true;
   killall SystemUIServer` ([ref][]).

[ref]: http://computers.tutsplus.com/tutorials/how-to-become-an-os-x-screenshot-wizard--mac-50467


### Other things:

 * It seems like [vcsh][] might be the eventual awesome way to do
   configs. With [myrepos][] probably. See: [Manage Your Configs
   with vcsh][]
 * There's a bunch more at [dotfiles.github.io][].


[vcsh]: https://github.com/RichiH/vcsh
[myrepos]: http://myrepos.branchable.com/
[Manage Your Configs with vcsh]: http://www.linuxjournal.com/content/manage-your-configs-vcsh
[dotfiles.github.io]: http://dotfiles.github.io/
