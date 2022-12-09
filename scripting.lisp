
(in-package :ciel)

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

(defun main ()
  "Run a lisp file as a script.
  If no argument is given or if the file doesn't exist, run the top-level CIEL

  The script should begin with:

    (in-package :ciel-user)

  We have two ways to run a CIEL script:

  1) by calling the ciel binary with a file as argument:

    $ ciel myscript.lisp

  2) by using a shebang. It's a little bit convoluted:

  #!/bin/sh
  #|-*- mode:lisp -*-|#
  #|
  exec /path/to/ciel `basename $0` \"$@\"
  (print \"hello CIEL!\")

  How it works:

  - it starts as a /bin/sh script
    - all lines starting by # are shell comments
  - the exec calls the ciel binary with this file name as first argument,
    the rest of the file (lisp code) is not read by the shell.
    - before LOAD-ing this Lisp file, we remove the #!/bin/sh shebang line.
    - Lisp ignore comments between #| and |#

  Exciting things to come!"
  ;; (format t "Hello main! ~S~&" uiop:*command-line-arguments*)
  (let ((file (first (uiop:command-line-arguments))))
    (cond
      ((and file
            (uiop:file-exists-p file))
       (if (has-shebang file)
           ;; I'm a bit cautious about this function.
           ;; (mostly, small issues when testing at the REPL, should be fine though…)
           (load-without-shebang file)
           ;; So the one with no risk:
           (load file)))
      (t
       (when (and file (not (uiop:file-exists-p file)))
         (format t "warn: file ~S does not exist.~&" file))
       (sbcli::repl)))))
