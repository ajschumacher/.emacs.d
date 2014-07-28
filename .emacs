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

;; elpy for python
(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)

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
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

;; set C-x g to magit
(global-set-key (kbd "C-x g") 'magit-status)

;; sometimes C-spc and C-@ don't work, so set mark this way too
(global-set-key (kbd "C-x 9") 'set-mark-command)

;; make C-h and M-h backspace; move help to C-x h
;; (on some systems, C-h already sends DEL)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x h") 'help-command)

;; I switch to other window a lot
(global-set-key (kbd "C-q") 'other-window)


;;; UI things for display

;; get rid of UI stuff (not sure all of these always apply)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn on time mode
(display-time-mode t)

;; turn off highlight-indentation-mode by making it not load by default
(delete 'highlight-indentation-mode elpy-default-minor-modes)

;; improve status line
(setq column-number-mode t)

;; get rid of those trailing dashes
(setq mode-line-end-spaces "")

;; diminish some things
(diminish 'undo-tree-mode)
(diminish 'compilation-shell-minor-mode)

;; highlight the current line
(global-hl-line-mode t)

;; set a color scheme
;;(load-theme 'misterioso)


;;; UI things for interaction

;; One space after sentences. One.
(setq sentence-end-double-space nil)

;; Precise when moving to next lines
(setq scroll-step 1)

;; Get help on flymake-discovered problems
(custom-set-variables
      '(help-at-pt-timer-delay 0.9)
           '(help-at-pt-display-when-idle '(flymake-overlay)))

;; Allow region downcase w/ C-x C-l
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e24180589c0267df991cf54bf1a795c07d00b24169206106624bb844292807b9" "572caef0c27b100a404db8d540fd5b31397f90ab660ef5539ff0863ff9bee26a" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "297063d0000ca904abb446944398843edaa7ef2c659b7f9087d724bf6f8c1d1f" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "e74d80bf86c7951b1a27994faa417f7e3b4a02f7a365ed224f032bd29f5d2d6d" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "a99e7c91236b2aba4cd374080c73f390c55173c5a1b4ac662eeb3172b60a9814" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "353861e69d6510a824905208f7290f90248f0b9354ee034fd4562b962790bdfc" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" default)))
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "SCRIPT"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

;; macro(s)
(fset 'py-execute
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("a ewb*Pytohon*b" 0 "%d")) arg)))
(global-set-key (kbd "C-x C-k 4") 'py-execute)

(fset 'poor-slides
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("^$." 0 "%d")) arg)))
(global-set-key (kbd "C-x C-k 3") 'poor-slides)

(fset 'poor-slides-back
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("^$.^$" 0 "%d")) arg)))
(global-set-key (kbd "C-x C-k 2") 'poor-slides-back)

(fset 'py-truth-execute
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote (" wb*Python*bb*scratch*(sleep-for) 1)bassert( ==       b*Python*: wb)" 0 "%d")) arg)))
(global-set-key (kbd "C-x C-k 5") 'py-truth-execute)

(fset 'py-run-deassert
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote (" wb*Python*bass" 0 "%d")) arg)))
(global-set-key (kbd "C-x C-k 6") 'py-run-deassert)

;; don't open images in emacs
(add-hook 'org-mode-hook '(lambda ()
  (setq org-file-apps (append '(("\\.png\\'" . default)
				("\\.jpg\\'" . default)) org-file-apps))
))
