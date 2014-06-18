;; Manually built .emacs config

;; from Yegge's post:
;; https://sites.google.com/site/steveyegge2/effective-emacs
;; C-x C-m does long command (like M-x)
(global-set-key "\C-x\C-m" 'execute-extended-command)
;; kill words like readline; preserve kill-region
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
;; get rid of UI stuff (not sure all of these always apply)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; something weird is going on with C-h
;; what has happened to the help functionality on Ubuntu?
