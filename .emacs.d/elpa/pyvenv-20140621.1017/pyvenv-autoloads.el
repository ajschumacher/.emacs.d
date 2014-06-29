;;; pyvenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pyvenv-restart-python pyvenv-mode pyvenv-workon
;;;;;;  pyvenv-deactivate pyvenv-activate) "pyvenv" "pyvenv.el" (21424
;;;;;;  37724 0 0))
;;; Generated autoloads from pyvenv.el

(autoload 'pyvenv-activate "pyvenv" "\
Activate the virtual environment in DIRECTORY.

\(fn DIRECTORY)" t nil)

(autoload 'pyvenv-deactivate "pyvenv" "\
Deactivate any current virtual environment.

\(fn)" t nil)

(autoload 'pyvenv-workon "pyvenv" "\
Activate a virtual environment from $WORKON_HOME.

\(fn NAME)" t nil)

(defvar pyvenv-mode nil "\
Non-nil if Pyvenv mode is enabled.
See the command `pyvenv-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-mode'.")

(custom-autoload 'pyvenv-mode "pyvenv" nil)

(autoload 'pyvenv-mode "pyvenv" "\
Global minor mode for pyvenv.

Will show the current virtual env in the mode line, and respect a
`pyvenv-workon' setting in files.

\(fn &optional ARG)" t nil)

(autoload 'pyvenv-restart-python "pyvenv" "\
Restart Python inferior processes.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pyvenv-pkg.el") (21424 37724 109857 0))

;;;***

(provide 'pyvenv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyvenv-autoloads.el ends here
