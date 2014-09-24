;; help us emacs config - you're our only hope


;;; packages and modes and so on

;; establish package system
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Interactively Do Things
(require 'ido)
(ido-mode t)

;; tree undo
(global-undo-tree-mode t)

;; auto-complete mode
(global-auto-complete-mode t)

;; multiple cursors!
(require 'multiple-cursors)
;; this is nicer than string-rectangle
(global-set-key (kbd "C-x r t") 'mc/edit-lines)
;; this is enough for most other functionality
(global-set-key (kbd "C-x C-x") 'mc/mark-more-like-this-extended)

;; elpy for python
(elpy-enable)
(elpy-use-ipython)

;; ess for R
(require 'ess-site)

;; global snippets
(yas-global-mode t)

;; copying in something to make ido work better
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; copying in something to make ido work better for M-x
;; Smex
(require 'smex)
(smex-initialize)


;;; keybindings

;; M-y now does interactive stuff
(browse-kill-ring-default-keybindings)

;; more for Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; I use these too much to struggle with them
(global-set-key (kbd "C-]") 'beginning-of-buffer)
(global-set-key (kbd "C-\\") 'end-of-buffer)

;; expand-region is that new hotness
(global-set-key (kbd "M-o") 'er/expand-region)

;; maybe I'll want line numbers sometimes
(global-set-key (kbd "M-1") 'linum-mode)

;; dired at point is nice
(global-set-key (kbd "C-x C-j") 'dired-at-point)

;; from elpy guide guy; this is pretty cool
(define-key global-map (kbd "C-c o") 'iedit-mode)

;; don't use tab for yasnippets, use shift-tab
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; set C-x g to magit
(global-set-key (kbd "C-x g") 'magit-status)

;; sometimes C-spc and C-@ don't work, so set mark this way too
(global-set-key (kbd "C-x 9") 'set-mark-command)

;; make C-h and M-h backspace; move help to C-x h
;; (on some systems, C-h already sends DEL)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x h") 'help-command)

;; take Yegge's advice on kill-region
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; I switch to other window a lot
(global-set-key (kbd "C-q") 'other-window)

;; and I like to switch buffers
(global-set-key (kbd "C-x b") 'fill-paragraph)
(global-set-key (kbd "M-q") 'ido-switch-buffer)


;;; UI things for display

;; get rid of UI stuff (not sure all of these always apply)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn on time mode
(display-time-mode t)

;; turn off highlight-indentation-mode by making it not load by default
(delete 'elpy-module-highlight-indentation elpy-modules)

;; improve status line
(setq column-number-mode t)

;; get rid of those trailing dashes
(setq mode-line-end-spaces "")

;; diminish some things
(diminish 'undo-tree-mode)
(diminish 'compilation-shell-minor-mode)
(diminish 'auto-complete-mode)

;; highlight the current line
(global-hl-line-mode t)

;; set a color scheme
(load-theme 'zenburn t)
;; disable with disable-theme


;;; UI things for interaction

;; One space after sentences. One.
(setq sentence-end-double-space nil)

;; Precise when moving to next lines
(setq scroll-step 1)

;; Get help on flymake-discovered problems
(custom-set-variables
      '(help-at-pt-timer-delay 0.9)
           '(help-at-pt-display-when-idle '(flymake-overlay)))

;; Allow region downcase w/ C-x C-l, upcase w/ C-x C-u
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Put backup files a little out of the way
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
            (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t          ; backup file the first time it is saved
      backup-by-copying t          ; don't clobber symlinks
      version-control t            ; version numbers for backup files
      delete-old-versions t        ; delete excess backup files silently
      delete-by-moving-to-trash t  ; system recycle bin or whatever
      auto-save-default t          ; auto-save every buffer that visits file
      vc-make-backup-files t       ; backup version-controlled files too
)

;; better buffer names when multiple files have the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; make it easier to run elisp in org mode
(setq org-confirm-elisp-link-function 'y-or-n-p)

;; don't open images in emacs
(add-hook 'org-mode-hook '(lambda ()
  (setq org-file-apps (append '(("\\.png\\'" . default)
				("\\.jpg\\'" . default)) org-file-apps))
))
