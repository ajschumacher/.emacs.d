;;; Configuration --- Summary
;;; Commentary:
;; This is configuration for Emacs.
;;; Code:

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
;; Bootstrap install of use-package,
;; which also installs diminish.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(setq use-package-always-ensure t)
;; After this, use-package will install things as needed.


;;; Set some defaults.

;; ThisIsFourWords
(global-subword-mode t)

;; Use shift-arrows for changing windows.
(windmove-default-keybindings)

;; Show where buffers end.
(setq-default indicate-empty-lines t)

;; Consider using abbreviations.
(abbrev-mode)

;; Be aware of whitespace.
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode)
(diminish 'global-whitespace-mode)

;; Don't insert tabs.
(setq-default indent-tabs-mode nil)

;; Use just 'y' or 'n', not 'yes' or 'no'.
(defalias 'yes-or-no-p 'y-or-n-p)
;; Do the same for running elisp in org-mode.
(setq org-confirm-elisp-link-function 'y-or-n-p)

;; Don't show so many stars in org-mode.
(setq org-hide-leading-stars t)

;; Improve mode-line:
;; Show system time.
(display-time-mode t)
;; Show column number.
(setq column-number-mode t)
;; Don't show trailing dashes.
(setq mode-line-end-spaces "")

;; Blink, don't beep.
;; (setq visible-bell t)
;; A good setting, but resulting in visual artifacts.

;; Delete marked region when typing over it.
(delete-selection-mode t)

;; Have nice parentheses.
(show-paren-mode t)
(electric-pair-mode t)

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

;; Use spell-checking.
;; The aspell executable may be in /usr/local/bin.
(setq exec-path (append exec-path '("/usr/local/bin")))
(add-hook 'text-mode-hook (lambda ()
                            (flyspell-mode)
                            (diminish 'flyspell-mode)))
(add-hook 'prog-mode-hook (lambda ()
                            (flyspell-prog-mode)
                            (diminish 'flyspell-mode)))

;; Show off lambdas everywhere.
(when (and (<= 24 emacs-major-version)
           (<= 4 emacs-minor-version))
  (define-globalized-minor-mode
    my-global-prettify-symbols-mode
    prettify-symbols-mode
    (lambda () (prettify-symbols-mode t)))
  (my-global-prettify-symbols-mode t)
  (defconst prettify-symbols-alist
    '(("lambda"  . ?λ))))

;; Put backup files a little out of the way.
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


;;; Set some keybindings.

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
(global-set-key (kbd "M-1") 'linum-mode)

;; switch point into buffer list
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; dired at point is nice
(global-set-key (kbd "C-x C-j") 'dired-at-point)

;; Make C-h and M-h backspace; move help to C-x h.
;; (On some systems, C-h already sends DEL.)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x h") 'help-command)


;; Use nice colors.
(use-package zenburn-theme
  :config (load-theme 'zenburn t))
;; Themes can be disabled with disable-theme.


;; Get useful line behaviors when region is not active.
(use-package whole-line-or-region
  :config (whole-line-or-region-mode t)
  :diminish whole-line-or-region-mode)


;; Work with git with magic ease.
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-push-always-verify nil)
  (set-default 'magit-unstage-all-confirm nil)
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-revert-buffers 'silent)
  ;; Don't use tabs, magit!
  (add-hook 'git-commit-mode-hook
            '(lambda () (untabify (point-min) (point-max))) t))


(add-to-list 'load-path
             (expand-file-name "git-gutter-plus" user-emacs-directory))
(require 'git-gutter+)
(global-git-gutter+-mode)
; change! clear!
;; Show where files have changed vs. last commit.
;; (use-package git-gutter-fringe+
;;   :ensure nil
;;   :init (global-git-gutter+-mode)
;;   :diminish git-gutter+-mode)


;; Interactive selection of things.
;; TODO: consider helm instead (see Sacha's config)
;; NOTE: "C-j: Use the current input string verbatim."
(ido-mode t)
(ido-everywhere t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(global-set-key (kbd "M-l") 'other-window)
(global-set-key (kbd "C-M-l") 'ido-switch-buffer)

;; list vertically (so much nicer!)
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package flx-ido
  :config (flx-ido-mode 1))

;; Smart M-x
(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  ;; take Yegge's advice and don't require M for M-x
  (global-set-key (kbd "C-x C-m") 'smex)
  ;; This is the old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


;; Add nice project functions for git repos.
(use-package projectile
  :config (projectile-global-mode)
  :diminish projectile-mode)


;; See the undo history and move through it.
(use-package undo-tree
  :config (global-undo-tree-mode t)
  :diminish undo-tree-mode)


;; Un-namespaced Common Lisp names.
;; https://github.com/browse-kill-ring/browse-kill-ring/pull/56
(require 'cl)
(use-package browse-kill-ring
  :config
  ;; Bind M-y to visual interactive kill ring.
  (browse-kill-ring-default-keybindings))


;; Get auto-complete functionality.
;; TODO: Determine whether this is doing what I really want.
(use-package auto-complete
  :config (global-auto-complete-mode t)
  :diminish auto-complete-mode)


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
(use-package buffer-stack
  :config
  (key-chord-define-global "jk" 'buffer-stack-down))


;; Move things around intuitively.
(use-package drag-stuff
  :config (drag-stuff-global-mode)
  :diminish drag-stuff-mode)


;; expand-region is that new hotness.
(use-package expand-region
  :bind ("M-o" . er/expand-region))


;; Work well with parentheses and friends.
;; TODO: Consider switching to smartparens.
(use-package wrap-region
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers '(("`" "`")))
  :diminish wrap-region-mode)


;; Conveniently zoom all of Emacs.
(use-package zoom-frm
  :bind
  ("C-=" . zoom-in/out)
  ("C-+" . zoom-in/out)
  ("C--" . zoom-in/out))


;; Search the web from Emacs.
(use-package engine-mode
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-/"))
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine google
    "https://www.google.com/#q=%s"
    :keybinding "g"))


;; Check syntax, make life better.
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (define-key flycheck-mode-map
    (kbd "C-c C-n")
    'flycheck-next-error)
  (define-key flycheck-mode-map
    (kbd "C-c C-p")
    'flycheck-previous-error)
  :diminish flycheck-mode)


;; Elpy the Emacs Lisp Python Environment.
(use-package elpy
  :config
  (elpy-enable)
  ;; Use ipython if available.
  (when (executable-find "ipython")
    (elpy-use-ipython))
  ;; Don't use flymake if flycheck is available.
  (when (require 'flycheck nil t)
    (setq elpy-modules
          (delq 'elpy-module-flymake elpy-modules)))
  ;; Don't use highlight-indentation-mode.
  (delete 'elpy-module-highlight-indentation elpy-modules)
  ;; this is messed with by emacs if you let it...
  (custom-set-variables
   '(elpy-rpc-backend "jedi")
   '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
   '(help-at-pt-timer-delay 0.9)
   '(tab-width 4))
  ;; Elpy also installs yasnippets.
  ;; Don't use tab for yasnippets, use shift-tab.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  :diminish elpy-mode)


;; Emacs Speaks Statistics includes support for R.
(use-package ess-site
  :ensure ess)


;; Use a nice JavaScript mode.
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))


;; See colors specified with text.
(use-package rainbow-mode
  :config
  (defun rainbow-mode-quietly ()
    (rainbow-mode)
    (diminish 'rainbow-mode))
  (add-hook 'html-mode-hook 'rainbow-mode-quietly)
  (add-hook 'css-mode-hook 'rainbow-mode-quietly))


;; Support markdown, for goodness sake.
(use-package markdown-mode)


;;; Functions Written by others:

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

(defun randomize-region (beg end)
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

(defun ajs-push-page-up ()
  (interactive)
  (scroll-up 1)
  ;; next-line is only meant for interactive use,
  ;; but it works really well here.
  (next-line))
(global-set-key (kbd "M-n") 'ajs-push-page-up)

(defun ajs-push-page-down ()
  (interactive)
  (scroll-down 1)
  ;; previous-line is only meant for interactive use,
  ;; but it works really well here.
  (previous-line))
(global-set-key (kbd "M-p") 'ajs-push-page-down)

(defun ajs-space-tab (current desired)
  "Change size of space tabs."
  (interactive "nCurrent size: \nnDesired size: ")
    (setq tab-width current)
    (tabify (point-min) (point-max))
    (setq tab-width desired)
    (untabify (point-min) (point-max))
    (setq tab-width desired)
    (setq python-indent-offset desired))

(defun ajs-decimal-escapes-to-unicode ()
  "Convert escapes like '&#955;' to Unicode like 'λ'.
Operates on the active region or the whole buffer."
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
  "Run the function in a buffer for the FILE and save it"
  (save-excursion
    (let ((buffer (find-file-noselect filename)))
      (message "Working on %s" filename)
      (set-buffer buffer)
      (funcall function)
      (save-buffer)
      (kill-buffer buffer))))

(defun ajs-run-in-many-files-and-save (list-of-filenames function)
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
