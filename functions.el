;; possibly a useful function
(defun space-tab (current desired)
  "Change size of space tabs."
  (interactive "nCurrent size: \nnDesired size: ")
    (setq tab-width current)
    (tabify (point-min) (point-max))
    (setq tab-width desired)
    (untabify (point-min) (point-max))
    (setq tab-width desired)
    (setq python-indent desired))

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
