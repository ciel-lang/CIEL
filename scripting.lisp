
(in-package :ciel)

(defparameter *scripts* (dict)
  "Available scripts.
  Hash-table: file name (sans extension) -> file content (string).")

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

;; eval
(defun wrap-user-code (s)
  "Wrap this user code to handle common conditions, such as a C-c C-c to quit gracefully."
  ;; But is it enough when we run a shell command?
  `(handler-case
       ,s                                ;; --eval takes one form only.
     (sb-sys:interactive-interrupt (c)
       (declare (ignore c))
       (format! *error-output* "Bye!~%"))
     (error (c)
       (format! *error-output* "~a" c))))


(defun register-builtin-scripts ()
  "Find available scripts in src/scripts, register them in *SCRIPTS*.
  Call this before creating the CIEL binary."
  ;; We save the file's content as a string.
  ;; We will run them with LOAD (and an input stream from the string).
  ;;
  ;; Example:
  ;;
  ;; (load (make-string-input-stream (str:from-file "src/scripts/simpleHTTPserver.lisp")))
  (loop for file in (uiop:directory-files "src/scripts/")
     if (equal "lisp" (pathname-type file))
     do (format t "~t scripts: registering ~a~&" (pathname-name file))
       (setf (gethash (pathname-name file) *scripts*)
             (str:from-file file))))

(defun run-script (name)
  "If NAME is registered in *SCRIPTS*, run this script."
  (bind (((:values content exists) (gethash name *scripts*)))
    (cond
      ((and exists (str:blankp content)
       (format *error-output* "uh the script ~s has no content?~&" name)))
      ((not exists)
       (format *error-output* "The script ~s was not found.~&" name))
      (t
       ;; Run it!
       (load (make-string-input-stream content))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ciel-user
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (in-package :ciel-user)

(defun main (&optional args)
  "Read optional command-line arguments, execute some lisp code or start a top-level REPL.

  # eval some lisp code

  Use --eval or -e. Example:

  $ ciel -e \"(uiop:file-exists-p \"README.org\")\"
  /home/vindarel/projets/ciel/README.org   <= the file name is returned, otherwise \"NIL\".

  # start the readline CIEL REPL

  If no argument is given or if the file given as argument doesn't exist, run the top-level CIEL

  The script should begin with:

    (in-package :ciel-user)

  We have two ways to run a CIEL script:

  1) by calling the ciel binary with a file as argument:

    $ ciel myscript.lisp

  2) by using a shebang.

  #!/usr/bin/env ciel
  (in-package :ciel-user)
  (print \"hello CIEL!\")

  Exciting things to come!"
  (let ((args (or args ;; for testing
                  (uiop:command-line-arguments))))

    (handler-case
        (loop
           :for arg = (first args) :do

             (cond
               ;; --eval, -e
               ((member arg '("--eval" "-e") :test #'equal)
                (pop args)
                (setf arg (first args))

                (handler-case
                    ;; I want to run this in :ciel-user,
                    ;; but to define these helper functions in :ciel.
                    (let ((*package* (find-package :ciel-user))
                          res)
                      (setf res
                            (eval
                             (wrap-user-code (read-from-string arg))))
                      (when res
                        ;; print aesthetically or respect lisp structure?
                        (format! t "~a~&" res)))
                  (end-of-file ()
                    (format! t "End of file error. Did you close all parenthesis?"))
                  (error (c)
                    (format! t "An error occured: ~a~&" c)))

                (return-from main))

               ;; --script / -s : run scripts by name.
               ;; They are registered by name in the binary.
               ;; Ideas:
               ;; - look for scripts in specified directories.
               ((member arg '("--script" "-s") :test #'equal)
                (pop args)
                (setf arg (first args))
                ;; ditch the "-s" option, must not be seen by the script.
                (pop uiop:*command-line-arguments*)
                (run-script arg)
                (return-from main))

               ;; LOAD some file.lisp
               ;; Originally, the goal of the scripting capabilities. The rest are details.
               ((and arg
                     (uiop:file-exists-p arg))
                (pop args)
                (if (has-shebang arg)
                    ;; I was a bit cautious about this function.
                    ;; (mostly, small issues when testing at the REPL because of packages and local nicknames,
                    ;; should be fine though…)
                    (load-without-shebang arg)
                    ;; So the one with no risk:
                    (load arg))
                (return-from main))

               ;; default: run CIEL's REPL.
               (t
                (when (and arg (not (uiop:file-exists-p arg)))
                  (format t "warn: file ~S does not exist.~&" arg)
                  (pop args))
                (sbcli::repl)
                )))

      (error (c)
        (format! *error-output* "Unexpected error: ~a~&" c)
        (return-from main)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; top-level for binary construction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~&Registering built-in scripts in src/scripts/ …~&")
(register-builtin-scripts)
