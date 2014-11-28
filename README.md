# My config files

### Setup

(The order is not always strict.)

 * Install `git`
 * In home directory, `git clone <clone URL>`
 * Run the `link.sh` script as needed to connect things.
 * Install `emacs` 24:
     * On Ubuntu etc.: `apt-get install emacs24`
     * On a Mac this is often okay; see also
       [Clojure's fine directions][]
         * `brew install emacs --HEAD --use-git-head --cocoa --with-gnutls`
           *DO IT*
         * `brew install emacs --cocoa --srgb`
         * `brew linkapps Emacs`
     * On a Mac this is (kind of) the bee's knees: [emacs-mac-port][]
         * `brew tap railwaycat/emacsmacport`
         * `brew install emacs-mac`
         * `brew linkapps Emacs`
         * You get nice things like more utf-8 characters actually
           displaying, etc.
 * Install [cask][] for `emacs` dependencies (use python installer with `curl -fsSkL https://raw.github.com/cask/cask/master/go | python` if it hasn't changed)
 * Install `emacs` package dependencies with `cask install` from
   `~/.emacs.d`
 * Install `aspell` to have spell checking in emacs (and everywhere)
 * `python` and `R` kind of go without saying... (`r-base` on Ubuntu)
 * `pip install elpy jedi` for all the emacs-python love from elpy
   (`rope` can be used in place of `jedi`)
 * `gem install rubocop ruby-lint pry pry-doc method_source` to get nice things for Ruby
 * possibly install `gnutls` (`brew install gnutls` on mac)

[Clojure's fine directions]: http://clojure-doc.org/articles/tutorials/emacs.html
[emacs-mac-port]: https://github.com/railwaycat/emacs-mac-port
[cask]: https://github.com/cask/cask


### Other Mac tweaks

 * `System Preferences...`, `Keyboard`, `Modifier Keys...`, `Caps Lock to
   Control`
 * The nice default-provided font in Terminal is called "Menlo".
 * To turn off window drop shadows when doing screenshots, run:
   `defaults write com.apple.screencapture disable-shadow -bool true;
   killall SystemUIServer` ([ref1][]).
 * To make all files visible in the Finder, run: `defaults write com.apple.finder AppleShowAllFiles TRUE; killall Finder` ([ref2][])

[ref1]: http://computers.tutsplus.com/tutorials/how-to-become-an-os-x-screenshot-wizard--mac-50467
[ref2]: https://discussions.apple.com/thread/1935221


### Browser stuff

 * For Chrome:
     * Install [AdBlock](https://chrome.google.com/webstore/detail/adblock/gighmmpiobklfepjocnamgkkbiglidom)
     * Install [Markdown Here](http://markdown-here.com/)
     * Install [Markdown Preview Plus](https://chrome.google.com/webstore/detail/markdown-preview-plus/febilkbfcbhebfnokafefeacimjdckgl)
     * Install [JSONView](https://chrome.google.com/webstore/detail/jsonview/chklaanhfefbnpoihckbnefhakgolnmc)
     * Install [Tampermonkey](https://chrome.google.com/webstore/detail/tampermonkey/dhdgffkkebhmkfjojejmpbldmpobfkfo)
     * Install [downloadyoutube](https://github.com/gantt/downloadyoutube)
     * Install [Hangouts](https://chrome.google.com/webstore/detail/hangouts/nckgahadagoaajjgafhacjanaoiihapd), I guess?


### Other things:

 * It seems like [vcsh][] might be the eventual awesome way to do
   configs. With [myrepos][] probably. See: [Manage Your Configs
   with vcsh][]
 * There's a bunch more at [dotfiles.github.io][].


[vcsh]: https://github.com/RichiH/vcsh
[myrepos]: http://myrepos.branchable.com/
[Manage Your Configs with vcsh]: http://www.linuxjournal.com/content/manage-your-configs-vcsh
[dotfiles.github.io]: http://dotfiles.github.io/
