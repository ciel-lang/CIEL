
(in-package :ciel)

(defun main ()
  "Run a lisp file as a script.
  If no file is given or if it doesn't exist, run the top-level CIEL REPL.

  It should begin with:

    (in-package :ciel-user)

  Exciting things to come!"
  ;; (format t "Hello main! ~S~&" uiop:*command-line-arguments*)
  (let ((file (first (uiop:command-line-arguments))))
    (cond
      ((and file
            (uiop:file-exists-p file))
       (load file))
      (t
       (sbcli::repl)))))
