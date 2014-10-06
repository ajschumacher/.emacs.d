;; help us emacs config - you're our only hope


;;; packages and modes and so on

;; establish package system
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; turn this on
(whole-line-or-region-mode t)

;; marks for changes
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)

;; you know what? why the heck not
(require 'nyan-mode)

;; Interactively Do Things
(require 'ido)
(ido-mode t)

;; tree undo
(global-undo-tree-mode t)

;; auto-complete mode
(global-auto-complete-mode t)

;; lines for ^L
(global-page-break-lines-mode t)

;; multiple cursors!
(require 'multiple-cursors)
;; this is nicer than string-rectangle
(global-set-key (kbd "C-x r t") 'mc/edit-lines)
;; this is enough for most other functionality
(global-set-key (kbd "C-x C-x") 'mc/mark-more-like-this-extended)

(require 'iy-go-to-char)
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

(require 'key-chord)
(key-chord-mode t)

(key-chord-define-global "hj" 'undo)
(key-chord-define-global "cv" 'iy-go-to-char)
(key-chord-define-global "fg" 'iy-go-to-char-backward)
(key-chord-define-global ",." 'backward-paragraph)
(key-chord-define-global "./" 'forward-paragraph)

;; elpy for python
(elpy-enable)
(elpy-use-ipython)
;; but use flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

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

;; and... twitter
(require 'twittering-mode)


;;; keybindings

;; use the Mac keys:
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)

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

;; Joy of paragraphs
(global-set-key (kbd "M-\\") 'mark-paragraph)

;; more mark-whole-buffer
(global-set-key (kbd "M-A") 'mark-whole-buffer)

;; expand-region is that new hotness
(global-set-key (kbd "M-o") 'er/expand-region)

;; maybe I'll want line numbers sometimes
(global-set-key (kbd "M-1") 'linum-mode)

;; dired at point is nice
(global-set-key (kbd "C-x C-j") 'dired-at-point)

;; from elpy guide guy; this is pretty cool
(define-key global-map (kbd "C-c C-o") 'iedit-mode)

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

;; Highlight ugly whitespace
(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode)

;; turn on time mode
(display-time-mode t)

;; turn off highlight-indentation-mode by making it not load by default
(delete 'elpy-module-highlight-indentation elpy-modules)

;; improve status line
(setq column-number-mode t)

;; get rid of those trailing dashes
(setq mode-line-end-spaces "")

;; 'after', from bling's config
(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;; diminish some things
(diminish 'undo-tree-mode)
(diminish 'compilation-shell-minor-mode)
(diminish 'auto-complete-mode)
(diminish 'whole-line-or-region-mode)
(diminish 'page-break-lines-mode)
(diminish 'global-whitespace-mode)
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'magit (diminish 'magit-auto-revert-mode))
(after 'wrap-region (diminish 'wrap-region-mode))
(after 'flycheck (diminish 'flycheck-mode))

;; set a color scheme
(load-theme 'zenburn t)
;; disable with disable-theme

;; frame zooming with zoom-frm
(require 'zoom-frm)
(global-set-key (kbd "C-=") 'zoom-in/out)
(global-set-key (kbd "C-+") 'zoom-in/out)
(global-set-key (kbd "C-0") 'zoom-in/out)
(global-set-key (kbd "C--") 'zoom-in/out)

;; don't beep all the time
(setq visible-bell t)
;; (doesn't apply to terminal mode)
;; (have to adjust a setting in the term)


;;; UI things for interaction

;; wrap things nicely
(wrap-region-global-mode t)
(wrap-region-add-wrappers
 '(("`" "`")))

;; Don't insert tabs!
(setq-default indent-tabs-mode nil)

;; just 'y' or 'n', not 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; parentheses (etc.)
(show-paren-mode t)
(electric-pair-mode t)

;; delete marked stuff
(delete-selection-mode t)

;; One space after sentences. One.
(setq sentence-end-double-space nil)

;; Precise when moving to next lines
(setq scroll-step 1)

;; this is messed with by emacs if you let it...
(custom-set-variables
 '(elpy-rpc-backend "jedi")
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(tab-width 4))

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


;;; randomize-reqion:

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

;;; randomize-region.el

;;; the open function from prelude
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
;;; end prelude open function


;; set up email maybe?

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "ajschumacher@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      user-full-name "Aaron Schumacher"
      user-mail-address "ajschumacher@gmail.com")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
