# My config files

Also:

 * install `aspell` to have spell checking in emacs (and everywhere)
 * `pip install elpy rope` for all the emacs-python love from elpy (`jedi` can be used in place of `rope`)
 * `python` and `R` kind of go without saying...
 * install `[cask](https://github.com/cask/cask)` for `emacs` dependencies (use python installer!)
 * install `emacs` package dependencies with `cask install` from `~/.emacs.d`

On a mac also:

 * `System Preferences...` `Keyboard` `Modifier Keys...` Caps Lock to Control
 * The nice default-provided font in Terminal is called "Menlo".
 * To turn off window drop shadows when doing screenshots, run: `defaults write com.apple.screencapture disable-shadow -bool true; killall SystemUIServer` ([ref](http://computers.tutsplus.com/tutorials/how-to-become-an-os-x-screenshot-wizard--mac-50467)).


### Other things to consider:

 * It seems like [vcsh][] might be the eventual awesome way to do
   configs. With [myrepos][] probably. See: [Manage Your Configs
   with vcsh][]


### References:

 * [http://dotfiles.github.io/][]


[KeyRemap4MacBook]: https://pqrs.org/macosx/keyremap4macbook/
[vcsh]: https://github.com/RichiH/vcsh
[myrepos]: http://myrepos.branchable.com/
[Manage Your Configs with vcsh]: http://www.linuxjournal.com/content/manage-your-configs-vcsh
[http://dotfiles.github.io/]: http://dotfiles.github.io/
