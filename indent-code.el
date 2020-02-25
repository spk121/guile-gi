:; exec emacs -Q --script $0 "$@"
;; "Language-agnostic" code indentation.
;; If you use this on src/, make sure to follow up with make indent,
;; because there are some things it doesn't pick up, such as max line length.

(mapc
 (lambda (file)
   (find-file file)
   (indent-region (point-min) (point-max))
   (untabify (point-min) (point-max))
   (delete-trailing-whitespace)
   (save-buffer)
   (kill-buffer))
 command-line-args-left)
