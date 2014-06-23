;; Manually built .emacs config

;; get rid of UI stuff (not sure all of these always apply)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; improve status line
(setq column-number-mode t)

;; make C-h and M-h backspace; move help to C-x h
;; (on some systems, C-h already sends DEL)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x h") 'help-command)

;; Prefer undo access and move upcase-word
(global-set-key (kbd "M-u") 'undo)
(global-set-key (kbd "C-x y") 'upcase-word)

;; One space after sentences. One.
(setq sentence-end-double-space nil)

;; Interactively Do Things
(require 'ido)
(ido-mode t)

;; Precise when moving to next lines
(setq scroll-step 1)

;; auto-complete mode
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; Allow region downcase w/ C-x C-l
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
