;;; early-init.el --- Runs before the GUI is set up  -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs 27 introduced this file.  It is read before the package system
;; comes up and before the first frame is created, which makes it the
;; right place to switch off frame decorations: doing it here means they
;; are never drawn, rather than drawn and then removed.
;;; Code:

;; Get rid of the chrome before the first frame appears.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq tool-bar-mode nil
      scroll-bar-mode nil)
;; The menu bar is free on a Mac -- it lives in the system menu bar
;; rather than taking a line of the frame -- so leave it alone there.
(unless (memq window-system '(mac ns))
  (push '(menu-bar-lines . 0) default-frame-alist)
  (setq menu-bar-mode nil))

;; package-initialize is called explicitly in init.el.
(setq package-enable-at-startup nil)

;; Startup is measurably faster with the garbage collector loosened up;
;; init.el puts it back to something sane once loading is done.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(provide 'early-init)
;;; early-init.el ends here
