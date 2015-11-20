;; Just a sec - have to clean things up a little!
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-startup-screen t)


;; Welcome!
(setq user-full-name "Aaron Schumacher"
      user-mail-address "ajschumacher@gmail.com")


;; This package called package comes with Emacs.
(require 'package)
;; Many packages are on MELPA.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; From github.com/magnars/.emacs.d:
;; Ensure we have MELPA package awareness.
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))
;; Turn on packaging.
(package-initialize)

;; From github.com/sachac/.emacs.d:
;; Bootstrap install of use-package.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
;; After this, use-package with :ensure will install if needed.

;; (Things are set up so that first startup will be slow!)

;; This promises to make things faster on second load.
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)


;; some defaults for new things
;; TODO: reconsider
(setq-default major-mode 'text-mode)


;; turn this on because... good.
(use-package whole-line-or-region
  :ensure t
  :config (whole-line-or-region-mode t)
  :diminish whole-line-or-region-mode)


;; shift-arrows for changing windows
(windmove-default-keybindings)


;; I guess I have to turn this on...
(abbrev-mode)


;; marks for changes
(use-package git-gutter-fringe+
  :ensure t
  :config (global-git-gutter+-mode t))


;; Interactive
;; TODO: consider helm instead (see Sacha's config)
(ido-mode t)
(ido-everywhere t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; list vertically (so much nicer!)
(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode t))

(use-package flx-ido
  :ensure t
  :config (flx-ido-mode 1))

;; Smart M-x
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  ;; take Yegge's advice and don't require M for M-x
  (global-set-key (kbd "C-x C-m") 'smex)
  ;; This is the old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


;; projectile adds nice project functions
(use-package projectile
  :ensure t
  :config (projectile-global-mode)
  :diminish projectile-mode)


;; tree undo
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t)
  :diminish undo-tree-mode)


;; ;; auto-complete mode
;; (global-auto-complete-mode t)


;; lines for ^L
(use-package page-break-lines
  :ensure t
  :config (global-page-break-lines-mode t)
  :diminish page-break-lines-mode)


;; ;; multiple cursors!
;; (require 'multiple-cursors)
;; ;; this is nicer than string-rectangle
;; (global-set-key (kbd "C-x r t") 'mc/edit-lines)
;; ;; this is enough for most other functionality
;; (global-set-key (kbd "C-x C-x") 'mc/mark-more-like-this-extended)

;; (require 'iy-go-to-char)
;; (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

;; (require 'key-chord)
;; (key-chord-mode t)

;; (key-chord-define-global "hj" 'undo)
;; ;; I hardly ever use this and want fg for window-switching
;; ;;(key-chord-define-global "fg" 'iy-go-to-char)
;; (key-chord-define-global "cv" 'iy-go-to-char-backward)
;; (key-chord-define-global "yu" 'backward-paragraph)
;; ;; conflicts with the word "column"
;; ;;(key-chord-define-global "nm" 'forward-paragraph)

;; ;; define some engines for engine-mode
;; (require 'engine-mode)
;; (engine-mode t)
;; (engine/set-keymap-prefix (kbd "C-/"))
;; (defengine github
;;   "https://github.com/search?ref=simplesearch&q=%s")
;; (defengine duckduckgo
;;   "https://duckduckgo.com/?q=%s"
;;   :keybinding "d")
;; (defengine google
;;   "https://www.google.com/#q=%s"
;;   :keybinding "g")


;; ;; flycheck is my boss
;; (add-hook 'after-init-hook 'global-flycheck-mode)


;; ;; elpy for python
;; (elpy-enable)
;; ; often prefer ipython, but default to always-present cpython
;; ; (elpy-use-ipython)
;; ;; but don't use flymake, since using flycheck
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; ;; turn off highlight-indentation-mode by making it not load by default
;; (delete 'elpy-module-highlight-indentation elpy-modules)

;; ;; this is messed with by emacs if you let it...
;; (custom-set-variables
;;  '(elpy-rpc-backend "jedi")
;;  '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
;;  '(help-at-pt-timer-delay 0.9)
;;  '(tab-width 4))


;; ;; ess for R
;; (require 'ess-site)


;; ;; global snippets
;; (yas-global-mode t)


;; ;; I hear js2 is the good js
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; (drag-stuff-global-mode)

;; ;; ThisIsFourWords
;; (global-subword-mode t)


;; ;;; keybindings

;; ;; 'after', from bling's config
;; (if (fboundp 'with-eval-after-load)
;;     (defmacro after (feature &rest body)
;;       "After FEATURE is loaded, evaluate BODY."
;;       (declare (indent defun))
;;       `(with-eval-after-load ,feature ,@body))
;;   (defmacro after (feature &rest body)
;;     "After FEATURE is loaded, evaluate BODY."
;;     (declare (indent defun))
;;     `(eval-after-load ,feature
;;        '(progn ,@body))))

;; ;; tab for completing things
;; (after 'emmet-mode
;;   (define-key emmet-mode-keymap (kbd "C-<tab>") 'emmet-expand-yas)
;;   (diminish 'emmet-mode))

;; ;; use the Mac keys:
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'super)
;; (setq ns-function-modifier 'hyper)
;; ;; on a Mac, command (meta) - space already does things
;; ;; and conrol - delete does the same thing as meta - delete
;; ;; so this is totally free for just-one-space
;; (global-set-key (kbd "C-<backspace>") 'just-one-space)


;; ;; follow Sacha's lead on this one:
;; (global-set-key (kbd "RET") 'newline-and-indent)

;; (require 'smartrep)
;; (require 'operate-on-number)
;; (smartrep-define-key global-map "C-c ."
;;   '(("+" . apply-operation-to-number-at-point)
;;     ("-" . apply-operation-to-number-at-point)
;;     ("*" . apply-operation-to-number-at-point)
;;     ("/" . apply-operation-to-number-at-point)
;;     ("\\" . apply-operation-to-number-at-point)
;;     ("^" . apply-operation-to-number-at-point)
;;     ("<" . apply-operation-to-number-at-point)
;;     (">" . apply-operation-to-number-at-point)
;;     ("#" . apply-operation-to-number-at-point)
;;     ("%" . apply-operation-to-number-at-point)
;;     ("'" . operate-on-number-at-point)))

;; ;; M-y now does interactive stuff
;; (browse-kill-ring-default-keybindings)

;; ;; I use these too much to struggle with them
;; (global-set-key (kbd "C-]") 'beginning-of-buffer)
;; (global-set-key (kbd "C-\\") 'end-of-buffer)

;; ;; Joy of paragraphs
;; (global-set-key (kbd "M-\\") 'mark-paragraph)

;; ;; more mark-whole-buffer
;; (global-set-key (kbd "M-A") 'mark-whole-buffer)

;; ;; expand-region is that new hotness
;; (global-set-key (kbd "M-o") 'er/expand-region)

;; maybe I'll want line numbers sometimes
(global-set-key (kbd "M-1") 'linum-mode)

;; and I definitely want to see where buffers end
(setq-default indicate-empty-lines t)

;; dired at point is nice
(global-set-key (kbd "C-x C-j") 'dired-at-point)

;; ;; from elpy guide guy; this is pretty cool
;; (define-key global-map (kbd "C-c C-o") 'iedit-mode)

;; ;; don't use tab for yasnippets, use shift-tab
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; ;; set C-x g to magit
;; (global-set-key (kbd "C-x g") 'magit-status)

;; ;; don't nag when pushing
;; (setq magit-push-always-verify nil)

;; ;; sometimes C-spc and C-@ don't work, so set mark this way too
;; (global-set-key (kbd "C-x 9") 'set-mark-command)

;; ;; make C-h and M-h backspace; move help to C-x h
;; ;; (on some systems, C-h already sends DEL)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-h") 'backward-kill-word)
;; (global-set-key (kbd "C-x h") 'help-command)

;; ;; I switch to other window a lot
;; ;; (don't use "df" because of the PDF format)
;; (key-chord-define-global "fg" 'other-window)
;; ;; Easier key-action:
;; (global-set-key (kbd "M-l") 'other-window)

;; ;; and I like to switch buffers
;; (key-chord-define-global "jk" 'buffer-stack-down)
;; (key-chord-define-global "m<" 'buffer-stack-up)
;; ;; Do I need to bind 'ido-switch-buffer over the default on C-x b?
;; ;; (global-set-key (kbd "C-q") 'ido-switch-buffer)
;; ;; Yes.
;; (key-chord-define-global "JK" 'ido-switch-buffer)
;; (global-set-key (kbd "C-M-l") 'ido-switch-buffer)

;; from Mickie's book
(setq apropos-sort-by-scores t)


;; ;;; UI things for display


;; ;; Highlight ugly whitespace
;; (setq whitespace-style '(face trailing tabs tab-mark))
;; (global-whitespace-mode)

;; ;; untabify for git committing
;; (add-hook 'git-commit-mode-hook
;;           '(lambda ()
;;              (untabify (point-min) (point-max)))
;;           t)

;; ;; so rainbow. wow.
;; (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
;;   (lambda () (rainbow-mode t)))
;; (my-global-rainbow-mode t)

;; ;; turn on time mode
;; (display-time-mode t)

;; ;; improve status line
;; (setq column-number-mode t)

;; ;; get rid of those trailing dashes
;; (setq mode-line-end-spaces "")

;; ;; prettify everywhere!
;; (when (and (<= 24 emacs-major-version)
;;            (<= 4 emacs-minor-version))
;;   (define-globalized-minor-mode
;;     my-global-prettify-symbols-mode
;;     prettify-symbols-mode
;;     (lambda () (prettify-symbols-mode t)))
;;   (my-global-prettify-symbols-mode t)
;;   (defconst prettify-symbols-alist
;;     '(("lambda"  . ?λ))))

;; ;; diminish some things
;; (diminish 'compilation-shell-minor-mode)
;; (diminish 'auto-complete-mode)
;; (diminish 'page-break-lines-mode)
;; (diminish 'global-whitespace-mode)
;; (diminish 'rainbow-mode)
;; (diminish 'drag-stuff-mode)
;; (after 'flyspell (diminish 'flyspell-mode))
;; (after 'git-gutter+ (diminish 'git-gutter+-mode))
;; (after 'flycheck (diminish 'flycheck-mode)
;;                  (define-key flycheck-mode-map
;;                    (kbd "C-c C-n") 'flycheck-next-error)
;;                  (define-key flycheck-mode-map
;;                    (kbd "C-c C-p") 'flycheck-previous-error))

;; ;; set a color scheme
;; (load-theme 'zenburn t)
;; ;; disable with disable-theme

;; ;; frame zooming with zoom-frm
;; (require 'zoom-frm)
;; (global-set-key (kbd "C-=") 'zoom-in/out)
;; (global-set-key (kbd "C-+") 'zoom-in/out)
;; (global-set-key (kbd "C-0") 'zoom-in/out)
;; (global-set-key (kbd "C--") 'zoom-in/out)

;; ;; don't beep all the time
;; (setq visible-bell nil) ; turned off for now
;; ;; (doesn't apply to terminal mode)
;; ;; (have to adjust a setting in the term)

;; ;; get spell-checking in graphical mode where path is weird
;; (setq exec-path (append exec-path '("/usr/local/bin")))
;; (add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)


;; ;;; UI things for interaction

;; ;; Don't insert tabs!
;; (setq-default indent-tabs-mode nil)

;; ;; four space tabs for javascript and CSS
;; (setq-default js2-basic-offset 4)
;; (setq css-indent-offset 4)

;; ;; just 'y' or 'n', not 'yes' or 'no'
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; ;; switch point into buffer list
;; (global-set-key (kbd "C-x C-b") 'buffer-menu)

;; ;;; parentheses etc.
;; (show-paren-mode t)
;; (electric-pair-mode t)
;; (wrap-region-global-mode t)
;; (wrap-region-add-wrappers '(("`" "`")))
;; (after 'wrap-region (diminish 'wrap-region-mode))

;; delete marked stuff
(delete-selection-mode t)

;; One space after sentences. One.
(setq sentence-end-double-space nil)

;; Precise when moving to next lines
(setq scroll-step 1)

;; Allow region downcase w/ C-x C-l, upcase w/ C-x C-u
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ;; Put backup files a little out of the way
;; (defvar --backup-directory (concat user-emacs-directory "backups"))
;; (if (not (file-exists-p --backup-directory))
;;             (make-directory --backup-directory t))
;; (setq backup-directory-alist `(("." . ,--backup-directory)))
;; (setq make-backup-files t          ; backup file the first time it is saved
;;       backup-by-copying t          ; don't clobber symlinks
;;       version-control t            ; version numbers for backup files
;;       delete-old-versions t        ; delete excess backup files silently
;;       delete-by-moving-to-trash t  ; system recycle bin or whatever
;;       auto-save-default t          ; auto-save every buffer that visits file
;;       vc-make-backup-files t       ; backup version-controlled files too
;; )

;; ;; better buffer names when multiple files have the same name
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)

;; ;; make it easier to run elisp in org mode
;; (setq org-confirm-elisp-link-function 'y-or-n-p)

;; ;; fewer stars everywhere
;; (setq org-hide-leading-stars t)

;; the open function from prelude
(defun prelude-open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))
(global-set-key (kbd "C-c o") 'prelude-open-with)
