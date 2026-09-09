;;; init.el --- Aaron's Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; This is configuration for Emacs.  Frame chrome and the garbage
;; collector are dealt with in early-init.el, which runs first.
;;; Code:

(setq inhibit-startup-screen t)


;; Welcome!
(setq user-full-name "Aaron Schumacher"
      user-mail-address "ajschumacher@gmail.com")


;;; Packages.

;; This package called package comes with Emacs.
(require 'package)
;; Many packages are on MELPA.  (melpa-stable was tried, but it is
;; sparse enough that it left this config frozen for years.)  GNU ELPA
;; is enabled by default and is where vertico, orderless, marginalia,
;; consult, diff-hl, undo-tree and rainbow-mode come from.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; The authoritative list of what this config needs.  `install.sh' runs
;; `package-install-selected-packages' against it on a new machine.
;; (buffer-stack is not here: it was dropped from every archive, so a
;; copy lives in elisp/.)
(setq package-selected-packages
      '(browse-kill-ring
        consult
        diff-hl
        diminish
        drag-stuff
        exec-path-from-shell
        expand-region
        js2-mode
        key-chord
        magit
        marginalia
        markdown-mode
        multiple-cursors
        orderless
        page-break-lines
        projectile
        rainbow-mode
        smartparens
        undo-tree
        vertico
        vertico-prescient
        whole-line-or-region
        yasnippet
        zenburn-theme))

;; Ensure package awareness before anything tries to install.
(unless (file-directory-p
         (expand-file-name "elpa/archives/melpa" user-emacs-directory))
  (package-refresh-contents))

;; use-package ships with Emacs as of 29, so there is nothing to
;; bootstrap any more.
(require 'use-package)
(unless (package-installed-p 'diminish)
  (package-install 'diminish))
(require 'diminish)
(setq use-package-always-ensure t)

;; Locally vendored elisp (buffer-stack).
(add-to-list 'load-path
             (expand-file-name "elisp" user-emacs-directory))

;; https://github.com/purcell/exec-path-from-shell
;; A GUI Emacs started from the Dock inherits a minimal PATH, so import
;; the shell's.  This has to happen before anything looks for an
;; executable -- notably aspell, below.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))


;;; Set some defaults.

;; ThisIsFourWords
(global-subword-mode t)

;; Show where buffers end.
(setq-default indicate-empty-lines t)

;; Consider using abbreviations.
;; (A bare `(abbrev-mode)' only toggled it in whatever buffer happened
;; to be current while init ran, which was never the one wanted.)
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)

;; Be aware of whitespace.
;; `show-trailing-whitespace' is a buffer-local built-in and does just
;; the one thing wanted here.  (The old `global-whitespace-mode' block
;; also turned on `tab-mark', which marked up every buffer.)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))
;; Clean it up on demand with M-x delete-trailing-whitespace.

;; Don't insert tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Use just 'y' or 'n', not 'yes' or 'no'.
(setq use-short-answers t)
;; Do the same for running elisp in org-mode.
(setq org-confirm-elisp-link-function 'y-or-n-p)

;; Don't show so many stars in org-mode.
(setq org-hide-leading-stars t)

;; Improve mode-line:
;; Show system time.
(display-time-mode t)
;; Show column number.
(column-number-mode t)
;; Don't show trailing dashes.
(setq mode-line-end-spaces "")

;; Delete marked region when typing over it.
(delete-selection-mode t)

;; One space after sentences. One.
(setq sentence-end-double-space nil)

;; Update the screen by one line, not one page.
(setq scroll-step 1)

;; Allow region downcase w/ C-x C-l, upcase w/ C-x C-u.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Make nice buffer names when multiple files have the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Remember minibuffer history between sessions.  This is also what
;; gives M-x its most-recently-used ordering now that smex is gone.
(savehist-mode t)

;; Remember recently opened files, which consult-buffer (C-M-l) then
;; offers alongside the live buffers.
(recentf-mode t)
(setq recentf-max-saved-items 200)

;; Reopen a file where it was left off.
(save-place-mode t)

;; Show what a half-typed key sequence could still turn into.
(which-key-mode)
(diminish 'which-key-mode)

;; Use spell-checking.
;; Pin the checker explicitly.  `ispell-program-name' is otherwise
;; guessed when ispell.el first loads, and if aspell is not visible on
;; `exec-path' at that moment it silently latches onto "ispell" and
;; every flyspell command fails afterwards.
(let ((aspell (or (executable-find "aspell")
                  ;; Apple silicon, then Intel, as a fallback.
                  (seq-find #'file-executable-p
                            '("/opt/homebrew/bin/aspell"
                              "/usr/local/bin/aspell")))))
  (when aspell
    (setq ispell-program-name aspell)))

(add-hook 'text-mode-hook (lambda ()
                            (flyspell-mode)
                            (diminish 'flyspell-mode)))
(add-hook 'prog-mode-hook (lambda ()
                            (flyspell-prog-mode)
                            (diminish 'flyspell-mode)))

;; flyspell only checks words as they are typed, so opening an existing
;; file shows nothing until it is edited.  Check what is already there.
;; Hang this on `find-file-hook' rather than `flyspell-mode-hook': the
;; latter also fires for the throwaway buffers used during byte
;; compilation, which made installing a package spell-check its source.
;; Defer to an idle moment so that opening a file stays instant.
(defvar ajs-flyspell-buffer-size-limit 30000
  "Largest buffer to spell-check on open.
Measured here, `flyspell-buffer' on a markdown buffer costs about
0.36s at 20,000 characters, 0.93s at 30,000 and 2.04s at 40,000.
Above this, use \\[flyspell-buffer] by hand.")

(defun ajs-flyspell-check-existing-text ()
  "Spell-check this file once Emacs is idle."
  (when (and (bound-and-true-p flyspell-mode)
             buffer-file-name
             ;; Big files make this slow enough to notice.
             (< (buffer-size) ajs-flyspell-buffer-size-limit))
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       1 nil
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (flyspell-buffer))))))))
(add-hook 'find-file-hook #'ajs-flyspell-check-existing-text)

;; Curly quotes while writing prose.  `electric-quote-mode' is built in
;; as of Emacs 25.  Neither setting below is its default, but together
;; they are what makes it useful: " curls as well as ', and a ' after a
;; word becomes an apostrophe rather than an opening quote, so "don't"
;; comes out right.  C-q ' still inserts a straight quote.
(declare-function markdown-code-block-at-point-p "markdown-mode" (&optional pos))
(declare-function markdown-inline-code-at-point-p "markdown-mode" (&optional pos))

(defvar ajs-electric-quote-exempt-files
  (rx string-start
      (or "COMMIT_EDITMSG" "MERGE_MSG" "TAG_EDITMSG" "NOTES_EDITMSG"
          "SQUASH_MSG" "EDIT_DESCRIPTION")
      string-end)
  "Files that are prose, but close enough to code to want straight quotes.
Magit's `git-commit-mode' is a minor mode enabled after
`text-mode-hook' has already run, so it cannot be tested for there;
match the file name instead.")

(defun ajs-inhibit-electric-quote-p ()
  "Non-nil at a point where quotes must stay straight.
Markdown code blocks and inline code are the cases that matter:
curling a quote inside a code sample corrupts it."
  (and (derived-mode-p 'markdown-mode)
       (or (markdown-code-block-at-point-p)
           (markdown-inline-code-at-point-p))))

(defun ajs-enable-electric-quote ()
  "Turn on curly quotes for prose in this buffer."
  (unless (and buffer-file-name
               (string-match-p ajs-electric-quote-exempt-files
                               (file-name-nondirectory buffer-file-name)))
    (setq-local electric-quote-replace-double t
                electric-quote-context-sensitive t)
    (add-hook 'electric-quote-inhibit-functions
              #'ajs-inhibit-electric-quote-p nil t)
    (electric-quote-local-mode 1)))
(add-hook 'text-mode-hook #'ajs-enable-electric-quote)

;; Put backup files a little out of the way.
(defvar ajs-backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p ajs-backup-directory))
    (make-directory ajs-backup-directory t))
(setq backup-directory-alist `(("." . ,ajs-backup-directory)))
(setq make-backup-files t          ; backup file the first time it is saved
      backup-by-copying t          ; don't clobber symlinks
      version-control t            ; version numbers for backup files
      delete-old-versions t        ; delete excess backup files silently
      delete-by-moving-to-trash t  ; system recycle bin or whatever
      auto-save-default t          ; auto-save every buffer that visits file
      vc-make-backup-files t       ; backup version-controlled files too
      )

;; Custom writes here instead of scribbling in this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;;; Set some keybindings.

;; Use shift-arrows for changing windows.
(windmove-default-keybindings)

;; Use Mac keys:
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)
(global-set-key (kbd "C-<backspace>") 'just-one-space)

;; Jump easily to beginning and end.
(global-set-key (kbd "C-]") 'beginning-of-buffer)
(global-set-key (kbd "C-\\") 'end-of-buffer)

;; Easily memorable whole-buffer selection.
(global-set-key (kbd "M-A") 'mark-whole-buffer)

;; Easily turn line numbers on and off.
;; (linum-mode is obsolete; this is its replacement.)
(global-set-key (kbd "M-1") 'display-line-numbers-mode)

;; switch point into buffer list
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; dired at point is nice
(global-set-key (kbd "C-x C-j") 'dired-at-point)

;; Make C-h and M-h backspace; move help to C-x h.
;; (On some systems, C-h already sends DEL.)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x h") 'help-command)

;; Other window, as ever.
(global-set-key (kbd "M-l") 'other-window)

;; Conveniently zoom all of Emacs.
;; (zoom-frm did this before; `global-text-scale-adjust' is built in as
;; of Emacs 29 and keeps reading further presses of =, + and -.)
(global-set-key (kbd "C-=") 'global-text-scale-adjust)
(global-set-key (kbd "C-+") 'global-text-scale-adjust)
(global-set-key (kbd "C--") 'global-text-scale-adjust)


;; Dashes.  Everywhere else on a Mac, Option+hyphen types an en dash and
;; Option+Shift+hyphen an em dash.  That does not happen here, because
;; `mac-option-modifier' is super above: Option is a modifier key and no
;; longer composes characters, so those two chords arrive as s-- and
;; s-_.  Bind them to do what the fingers already expect.  (s-- was
;; `text-scale-adjust', which is redundant when zooming is on C-= and
;; C--.)  The long way round is still C-x 8 _ n and C-x 8 _ m.
(defun ajs-insert-en-dash ()
  "Insert an en dash, which is the one that goes between dates."
  (interactive)
  (insert ?\N{EN DASH}))

(defun ajs-insert-em-dash ()
  "Insert an em dash, the long one used as punctuation."
  (interactive)
  (insert ?\N{EM DASH}))

(defun ajs-insert-ellipsis ()
  "Insert a real ellipsis character, rather than three periods."
  (interactive)
  (insert ?\N{HORIZONTAL ELLIPSIS}))

(global-set-key (kbd "s--") 'ajs-insert-en-dash)
(global-set-key (kbd "s-_") 'ajs-insert-em-dash)
;; Option+semicolon, likewise.
(global-set-key (kbd "s-;") 'ajs-insert-ellipsis)


;;; Packages, configured.

;; Highlight where matching parens are.
(show-paren-mode t)
;; `smartparens` manages parens well.
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  ;; C-M-j isn't standard, but C-M-d doesn't work for me.
  (define-key smartparens-mode-map (kbd "C-M-j") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
  (define-key smartparens-mode-map (kbd "C-M-[") 'sp-rewrap-sexp)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-backward-unwrap-sexp)
  ;; markdown-mode
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*"
                   :wrap "C-*"
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :post-handlers '(("[d1]" "SPC"))
                   :skip-match 'sp--gfm-skip-asterisk)
    (sp-local-pair "**" "**")
    (sp-local-pair "_" "_" :wrap "C-_" :unless '(sp-point-after-word-p)))
  (defun sp--gfm-skip-asterisk (_ms mb _me)
    (save-excursion
      (goto-char mb)
      (save-match-data (or (looking-at "^\\* ")
                           (looking-at "^ \\* ")))))
  ;; Don't highlight when wrapping.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  :diminish smartparens-mode)


;; Move things around intuitively, with M-<up> and friends.
;; `drag-stuff-define-keys' has to be called explicitly: until 2016 the
;; minor mode called it itself when it turned on, so the arrow keys came
;; for free, and the config never mentioned them.  Now the mode installs
;; an empty keymap unless asked.
(use-package drag-stuff
  :config
  (drag-stuff-global-mode)
  (drag-stuff-define-keys)
  :diminish drag-stuff-mode)


;; expand-region is that new hotness.
(use-package expand-region
  :bind ("M-o" . er/expand-region))


;; Use nice colors.
(use-package zenburn-theme
  :config (load-theme 'zenburn t))
;; Themes can be disabled with disable-theme.


;; Get useful line behaviors when region is not active.
(use-package whole-line-or-region
  :config (whole-line-or-region-global-mode t)
  :diminish whole-line-or-region-local-mode)


;; Work with git with magic ease.
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  ;; Don't use tabs, magit!
  (add-hook 'git-commit-mode-hook
            (lambda () (untabify (point-min) (point-max))) t))


;; Show git changes in the fringe.
;; (This replaces a vendored fork of git-gutter+ and a hand-rolled
;; projectile-based refresh hook; diff-hl talks to magit directly.)
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  ;; The fringe is only there in a graphical frame.
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))


;;; Interactive selection of things.
;; This replaces ido + ido-vertical + flx-ido + smex.  The keys are the
;; same ones those had.

(use-package vertico
  :config (vertico-mode))

;; Match on space-separated pieces, in any order.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  ;; `orderless-flex' is what brings back flx-ido's partial matching:
  ;; the letters just have to appear in order, so "ajsdw" finds
  ;; `ajs-double-width'.  Literal and regexp are tried first, so exact
  ;; substrings still win.
  (setq orderless-matching-styles
        '(orderless-literal orderless-regexp orderless-flex))
  ;; Let orderless apply to file names too, alongside the built-in
  ;; styles that make partial path components (like /u/s/b) work.
  (setq completion-category-overrides
        '((file (styles basic partial-completion orderless)))))

;; Annotate what is being completed.
(use-package marginalia
  :config (marginalia-mode))

;; Rank candidates by how recently and how often they have been used,
;; which is the part of flx-ido/smex that orderless does not do on its
;; own.  Filtering stays with orderless; prescient only sorts.
(use-package vertico-prescient
  :after vertico
  :config
  (setq vertico-prescient-enable-filtering nil
        vertico-prescient-enable-sorting t)
  (vertico-prescient-mode)
  ;; Remember the rankings between sessions.
  (prescient-persist-mode))

(use-package consult
  :config
  ;; NOTE: for the old ido habit, "M-n" inserts the thing at point.
  (global-set-key (kbd "C-M-l") 'consult-buffer))

;; Smart M-x, without smex: `savehist-mode' above remembers what has
;; been run, and vertico floats those to the top.
(global-set-key (kbd "M-x") 'execute-extended-command)
;; take Yegge's advice and don't require M for M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; Just this mode's commands (what smex-major-mode-commands did).
(global-set-key (kbd "M-X") 'execute-extended-command-for-buffer)


;; Add nice project functions for git repos.
;; The C-c p prefix also has to be asked for now: projectile used to
;; bind it from a `projectile-keymap-prefix' defcustom, which is gone.
(use-package projectile
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :diminish projectile-mode)


;; See the undo history and move through it.
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  ;; Otherwise it litters .~undo-tree~ files next to everything.
  (setq undo-tree-auto-save-history nil)
  :diminish undo-tree-mode)


;; browse-kill-ring used to need the un-namespaced Common Lisp names
;; (see browse-kill-ring/browse-kill-ring#56), but that was fixed
;; upstream long ago.
(use-package browse-kill-ring
  :config
  ;; Makes M-y run browse-kill-ring when the last command wasn't a yank.
  (browse-kill-ring-default-keybindings))


;; Display lines for ^L characters.
(use-package page-break-lines
  :config (global-page-break-lines-mode t)
  :diminish page-break-lines-mode)


;; Edit in multiple places at the same time.
(use-package multiple-cursors
  :bind
  ("C-x r t" . mc/edit-lines)
  ("C-x C-x" . mc/mark-more-like-this-extended))


;; (Near) simultaneous keypresses create new keys.
(use-package key-chord
  :config
  (key-chord-mode t)
  (key-chord-define-global "hj" 'undo))


;; Flip through buffers with ease.
;; buffer-stack is vendored in elisp/ -- see the note in that file.
(use-package buffer-stack
  :ensure nil
  :config
  (key-chord-define-global "jk" 'buffer-stack-down))


;; Snippets.  (These used to arrive via elpy.)
(use-package yasnippet
  :config
  (yas-global-mode 1)
  ;; Don't use tab for yasnippets, use shift-tab.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  :diminish yas-minor-mode)


;; Check syntax, make life better.
;; flymake is built in and is what eglot reports through, so there is
;; no reason to run flycheck alongside it any more.  Same keys.
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c C-n" . flymake-goto-next-error)
              ("C-c C-p" . flymake-goto-prev-error))
  :diminish flymake-mode)


;; Python, via the built-in LSP client instead of elpy.
;; Needs a language server on PATH; the Brewfile installs
;; python-lsp-server.  Without one, python-mode still works, eglot just
;; doesn't start.
(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)))

;; Use the tree-sitter Python mode when its grammar is actually
;; installed, so this degrades quietly on a machine where it isn't.
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")))
(when (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'python))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))


;; Use a nice JavaScript mode.
(use-package js2-mode
  :mode "\\.js\\'")


;; See colors specified with text.
(use-package rainbow-mode
  :config
  (defun rainbow-mode-quietly ()
    (rainbow-mode)
    (diminish 'rainbow-mode))
  (add-hook 'html-mode-hook 'rainbow-mode-quietly)
  (add-hook 'css-mode-hook 'rainbow-mode-quietly))


;; Support markdown, for goodness sake.
(use-package markdown-mode
  :config
  (define-key markdown-mode-map (kbd "M-n") nil)
  (define-key markdown-mode-map (kbd "M-p") nil))

;; markdown-mode's italic matcher is quadratic on the wrong kind of
;; file.  For every `_' or `*' it asks whether that position is inside
;; inline code, and that check rescans the enclosing block from the
;; start each time.  A long document with few blank lines is one huge
;; block, so a link list with a couple of thousand underscores in it
;; costs about eight seconds per refontification -- which jit-lock does
;; while typing, so every keystroke stalls.
;;
;; Size alone is a bad predictor: a 77KB prose file here fontifies in
;; 0.06s while a 78KB link list takes 8.3s.  What matters is how many
;; candidates share a block, so estimate that directly.  It is cheap
;; (about 2ms) and separates the real files here by a factor of ~850.
(defvar ajs-markdown-italic-cost-limit 1000000
  "Rough cost above which `markdown-match-italic' is dropped.
Measured here, fontification takes roughly cost/1.2e7 seconds, so
this is about a tenth of a second.  The markdown files on this
machine score between 37,000 and 222,000 except for the one link
list that prompted all this, which scores 190 million.
See `ajs-markdown-italic-cost'.  Set to nil to never drop it.")

(defun ajs-markdown-italic-cost ()
  "Estimate what italic fontification will cost in this buffer.
Sums, over each blank-line-separated block, the number of italic
candidates in the block times the block's length."
  (save-excursion
    (save-match-data
      (let ((cost 0) (start (point-min)))
        (goto-char (point-min))
        (while (< start (point-max))
          (let* ((end (if (re-search-forward "^[ \t]*$" nil t) (point) (point-max)))
                 (len (- end start))
                 (candidates 0))
            (save-excursion
              (goto-char start)
              (while (re-search-forward "[_*]" end t)
                (setq candidates (1+ candidates))))
            (setq cost (+ cost (* candidates len))
                  start (max end (1+ start)))
            (goto-char start)))
        cost))))

(defun ajs-markdown-disable-italic-fontification ()
  "Drop `markdown-match-italic' from font-lock in this buffer."
  (interactive)
  (setq-local markdown-mode-font-lock-keywords
              (seq-remove (lambda (keyword)
                            (eq (car-safe keyword) 'markdown-match-italic))
                          markdown-mode-font-lock-keywords))
  (font-lock-refresh-defaults))

(defun ajs-markdown-maybe-disable-italic-fontification ()
  "Turn off italic fontification when it would make typing crawl."
  (when ajs-markdown-italic-cost-limit
    (let ((cost (ajs-markdown-italic-cost)))
      (when (> cost ajs-markdown-italic-cost-limit)
        (ajs-markdown-disable-italic-fontification)
        (message "Italic fontification off in %s: too slow (cost %d)."
                 (buffer-name) cost)))))
(add-hook 'markdown-mode-hook
          #'ajs-markdown-maybe-disable-italic-fontification)


;;; Functions Written by others:

(defun prelude-open-with ()
  "Open the underlying file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))
(global-set-key (kbd "C-c o") 'prelude-open-with)

(defun shuffle-list (list)
  "Randomly permute the elements of LIST.
All permutations equally likely."
  (let ((i 0)
        j
        temp
        (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setcar (nthcdr i list) (nth j list))
      (setcar (nthcdr j list) temp)
      (setq i (1+ i))))
  list)

(defun randomize-region (beg end)
  "Randomly permute the lines between BEG and END."
  (interactive "r")
  (if (> beg end)
      (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; put beg at the start of a line and end and the end of one --
    ;; the largest possible region which fits this criteria
    (goto-char beg)
    (or (bolp) (forward-line 1))
    (setq beg (point))
    (goto-char end)
    ;; the test for bolp is for those times when end is on an empty
    ;; line; it is probably not the case that the line should be
    ;; included in the reversal; it isn't difficult to add it
    ;; afterward.
    (or (and (eolp) (not (bolp)))
        (progn (forward-line -1) (end-of-line)))
    (setq end (point-marker))
    (let ((strs (shuffle-list
                 (split-string (buffer-substring-no-properties beg end)
                               "\n"))))
      (delete-region beg end)
      (dolist (str strs)
        (insert (concat str "\n"))))))

;; the-the in honor of An Introduction to Programming in Emacs Lisp
(defun the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))


;;; Functions written by me:

(defun ajs-set-width (num-cols)
  "Set the selected frame's width to NUM-COLS."
  (set-frame-width (selected-frame) num-cols))

(defun ajs-double-width ()
  "Widen the frame to two columns' worth."
  (interactive)
  (ajs-set-width 168))

(defun ajs-single-width ()
  "Narrow the frame to one column's worth."
  (interactive)
  (ajs-set-width 84))

(defun ajs-pull-up-next-line ()
  "Join the next line onto this one, leaving a single space."
  (interactive)
  (delete-indentation t)
  (when (looking-at " ")
    (delete-char 1)))
(global-set-key (kbd "C-M-o") 'ajs-pull-up-next-line)

(defun ajs-push-page-up ()
  "Scroll the page up by one line, keeping point where it is."
  (interactive)
  (scroll-up 1)
  ;; next-line is only meant for interactive use,
  ;; but it works really well here.
  (next-line))
(global-set-key (kbd "M-n") 'ajs-push-page-up)

(defun ajs-push-page-down ()
  "Scroll the page down by one line, keeping point where it is."
  (interactive)
  (scroll-down 1)
  ;; previous-line is only meant for interactive use,
  ;; but it works really well here.
  (previous-line))
(global-set-key (kbd "M-p") 'ajs-push-page-down)

(defun ajs-space-tab (current desired)
  "Change space-tab size from CURRENT to DESIRED."
  (interactive "nCurrent size: \nnDesired size: ")
  (setq tab-width current)
  (tabify (point-min) (point-max))
  (setq tab-width desired)
  (untabify (point-min) (point-max))
  (setq tab-width desired)
  (setq python-indent-offset desired))

(defun ajs-decimal-escapes-to-unicode ()
  "Convert decimal HTML escapes in the region or buffer to Unicode."
  (interactive)
  (let ((start (point)) (end (mark)))
    (or (use-region-p)
        (setq start (point-min) end (point-max)))
    (insert (replace-regexp-in-string
             "&#[0-9]*;"
             (lambda (match)
               (format "%c" (string-to-number (substring match 2 -1))))
             (filter-buffer-substring start end t)))))

(defun ajs-run-in-file-and-save (filename function)
  "Run FUNCTION in a buffer for FILENAME and save it."
  (save-excursion
    (let ((buffer (find-file-noselect filename)))
      (message "Working on %s" filename)
      (set-buffer buffer)
      (funcall function)
      (save-buffer)
      (kill-buffer buffer))))

(defun ajs-run-in-many-files-and-save (list-of-filenames function)
  "Run FUNCTION in a buffer for each of LIST-OF-FILENAMES and save."
  (dolist (filename list-of-filenames)
    (ajs-run-in-file-and-save filename function)))

(defun ajs-md-html-files-in-below-directory (directory)
  "List the .md and .html files in DIRECTORY and in its sub-directories."
  (interactive "DDirectory name: ")
  (let (all-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (while current-directory-list
      (cond
       ((or
         (equal ".md" (substring (car (car current-directory-list)) -3))
         (equal ".html" (substring (car (car current-directory-list)) -5)))
        (setq all-files-list
              (cons (car (car current-directory-list)) all-files-list)))
       ((eq t (car (cdr (car current-directory-list))))
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ()
          (setq all-files-list
                (append
                 (ajs-md-html-files-in-below-directory
                  (car (car current-directory-list)))
                 all-files-list)))))
      (setq current-directory-list (cdr current-directory-list)))
    all-files-list))


;;; Wrap up.

;; Run a server, so that `emacsclient' has something to talk to.
;; .gitconfig sets core.editor to emacsclient; without a server its
;; --alternate-editor fallback quietly started a whole second Emacs for
;; every commit message.
;; Not under --batch: install.sh loads this file that way, and a server
;; started there just leaves a stale socket behind for the next real
;; Emacs to trip over.
(require 'server)
(unless (or noninteractive (server-running-p))
  (server-start))

(when (file-exists-p custom-file)
  (load custom-file))

;; early-init.el turned the garbage collector way down for startup.
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
