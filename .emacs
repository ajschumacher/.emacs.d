;; Manually built .emacs config

;; C-x C-m does long command (like M-x)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

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
