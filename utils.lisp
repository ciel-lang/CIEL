(in-package :ciel)


;;; Utilities that are useful enough to be available everywhere.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These are used for the scripting capabilities.
;;; We can load a file with or without a shebang line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun maybe-ignore-shebang (in)
  "If this file starts with #!, delete the shebang line,
  so we can LOAD the file.
  Return: a stream (it is LOADable)."
  ;; thanks Roswell for the trick.
  (let ((first-line (read-line in)))
    (make-concatenated-stream
     ;; remove shebang:
     (make-string-input-stream
      (format nil "~a"
              (if (str:starts-with-p "#!" first-line)
                  ""
                  first-line)))
     ;; rest of the file:
     in)))

(defun load-without-shebang (file)
  "LOAD this file, but exclude the first line if it is a shebang line."
  (with-open-file (file-stream file)
    (load
     (maybe-ignore-shebang file-stream))))

(defun has-shebang (file)
  "Return T if the first line of this file is a shell shebang line (starts with #!)."
  (with-open-file (s file)
    (str:starts-with-p "#!" (read-line s))))
