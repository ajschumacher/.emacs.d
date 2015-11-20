;; Written by others!

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


;; Written by me!

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
