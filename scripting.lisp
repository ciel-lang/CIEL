
(in-package :ciel)

(defparameter *ciel-version* "0.1" "Read from .asd or version.lisp-expr file.")

(defparameter *scripts* (dict 'equalp)
  "Available scripts.
  Hash-table: file name (sans extension) -> file content (string).
  The name is case-insensitive (it's easier for typing things in the terminal).")

;; eval
(defun wrap-user-code (s)
  "Wrap this user code to handle common conditions, such as a C-c C-c to quit gracefully."
  ;; But is it enough when we run a shell command?
  `(handler-case
       ,s                                ;; --eval takes one form only so no need of ,@
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
       ;; We first add a symbol in the feature list, so a script nows when it is being executed.
       (push :ciel ciel-user::*features*)
       ;; We ignore the shebang line, if there is one.
       ;; We can call scripts either with ciel -s <name> or with ./script
       (load (maybe-ignore-shebang
              (make-string-input-stream content)))))))

(defun top-level/command ()
  "Creates and returns the top-level command"
  (clingon:make-command
   :name "ciel"
   :description "CIEL Is an Extended Lisp. It's Common Lisp, batteries included."
   :version *ciel-version*
   :license "todo"
   :authors '("vindarel <vindarel@mailz.org>")
   :usage (format nil "accepts optional command-line arguments.~%
~t~tWith no arguments, run the CIEL readline REPL.~%
~t~tWith a file as argument, run it as a script.~%
~t~tWith --eval / -e <FORM>, eval a Lisp form.~%
~t~tWith --script / -s <SCRIPT>, run a a script by its name. See --scripts to list the available scripts.")
   :options (top-level/options)
   :handler #'top-level/handler
   :sub-commands (top-level/sub-commands)))

(defun top-level/options ()
  "Creates and returns the options for the top-level command"
  (list
   (clingon:make-option
    :counter
    :description "verbosity level"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)
   (clingon:make-option
    :string
    :description "eval a lisp form"
    :short-name #\e
    :long-name "eval"
    :key :eval)
   (clingon:make-option
    :filepath
    :description "run a lisp file"
    :short-name #\s
    :long-name "script"
    :key :script)
   (clingon:make-option
    :flag
    :description "list available scripts."
    :long-name "scripts"
    :short-name #\z
    :key :scripts)
   (clingon:make-option
    :flag
    :description "Don't load the ~/.cielrc init file at start-up (for the CIEL terminal REPL)."
    :long-name "no-userinit"
    :key :no-userinit)
   (clingon:make-option
    :flag
    :description "Don't print the welcome banner."
    :long-name "noinform"
    :key :noinform)
   ))

#+(or)
(progn
  ;; Try options parsing:
  (clingon:parse-command-line (top-level/command) (list "-s" "myscript" "9999"))
  ;; More free args work:
  (clingon:parse-command-line (top-level/command) (list "-s" "myscript" "9999" "80" "b"))
  ;; But not other CLI options (that I'd like to pass to myscript):
  (clingon:parse-command-line (top-level/command) (list "-s" "myscript" "-b"))
  ;; To pass options to myscript, use "--":
  (clingon:parse-command-line (top-level/command) (list "-s" "myscript" "--" "-b")))


(defun top-level/handler (cmd)
  "The top-level handler: read optional command-line arguments, execute some lisp code or start a top-level REPL.

  # eval some lisp code

  Use --eval or -e.

  # start the readline CIEL REPL

  If no argument is given or if the file given as argument doesn't exist, run the top-level CIEL

  We have two ways to run a CIEL script:

  1) by calling the ciel binary with a file as argument:

  2) by using a shebang line."

  ;; XXX: it might be better to NOT use Clingon: we want to be able to pass remaining options
  ;; to the script.
  ;; ciel -s simpleHTTPserver 9999 => OK
  ;; ciel -s simpleHTTPserver 9999 -h => clingon fails with "unkown option -h of kind short".
  (let ((args (clingon:command-arguments cmd))
        (user (clingon:getopt cmd :user))
        (eval-string (clingon:getopt cmd :eval))
        (script-name (clingon:getopt cmd :script))
        (scripts (clingon:getopt cmd :scripts))
        (short-help (clingon:getopt cmd :short-help))
        (verbose (clingon:getopt cmd :verbose)))

    (handler-case
        (cond

          ;;
          ;; --eval, -e
          ;;

          (eval-string
           (handler-case
               ;; I want to run this in :ciel-user,
               ;; but to define these helper functions in :ciel.
               (let ((*package* (find-package :ciel-user))
                     res)
                 (setf res
                       (eval
                        (wrap-user-code (read-from-string eval-string))))
                 (when res
                   ;; print aesthetically or respect lisp structure?
                   (format! t "~a~&" res)))
             (end-of-file ()
               (format! t "End of file error. Did you close all parenthesis?"))
             (error (c)
               (format! t "An error occured: ~a~&" c)))

           (return-from top-level/handler))

          ;;
          ;; --script / -s : run scripts by name.
          ;;
          ;; They are registered by name in the binary.
          ;; Ideas:
          ;; - look for scripts in specified directories.
          (script-name
           ;; ditch the "-s" option, must not be seen by the script.
           (pop uiop:*command-line-arguments*)
           (let ((dir (uiop:getcwd))
                 ;; Here args, the free args, is a list of remaining CLI parameters, sans the script name.
                 ;; We want to pass it along to the script too, to be coherent with loading
                 ;; a file directly. Indeed, calling
                 ;; $ ./simpleHTTPserver.lisp 4242
                 ;; is equal to "ciel simpleHTTPserver.lisp 4242" under the hood,
                 ;; which is 2 free args, with the script name.
                 ;; It's important because scripts could rely on the arguments order
                 ;; and we want to be able to parse this script's args with Clingon, in both cases.
                 (ciel-user:*script-args* (push script-name args)))
             (uiop:with-current-directory (dir)
               (run-script script-name)))
           (return-from top-level/handler))

          ;;
          ;; --scripts : list available scripts (helper command).
          ;;
          (scripts
           (format t "CIEL v~a~%~%" *ciel-version*)
           (format t "Available scripts:~&")
           (do-hash-table (k v *scripts*)
             (format t "~t - ~a~&" k))

           (format! t "~%See: https://ciel-lang.github.io/CIEL/#/scripting~&")
           (return-from top-level/handler))

          ;;
          ;; Free args: run (LOAD) a file.
          ;;
          ;; First, check the file exists.
          ((and args
                (not (uiop:file-exists-p (first args))))
           (format t "file ~S does not exist.~&" (first args))
           (return-from top-level/handler))

          ;; LOAD some file.lisp
          ;; Originally, this is the goal of the scripting capabilities. The rest are details.
          ((and (first args)
                (uiop:file-exists-p (first args)))
           ;; Add a symbol in the feature list, so a script knows when it is being executed.
           (push :ciel ciel-user::*features*)

           ;; The remaining free args are passed along to our script's arguments.
           ;; Here the file name is already a free arg, so args equals something like
           ;; '("simpleHTTPserver.lisp" "4242") aka it has the file name.
           (let ((ciel-user:*script-args* args)
                 (*package* (find-package :ciel-user)))
             (if (has-shebang (first args))
                 ;; I was a bit cautious about this function.
                 ;; (mostly, small issues when testing at the REPL because of packages and local nicknames,
                 ;; should be fine though…)
                 (load-without-shebang (first args))
                 ;; So the one with no risk:
                 (load (first args)))
             (return-from top-level/handler)))

          ;; default: run CIEL's REPL.
          (t
           ;; XXX: maybe pass all CLI options here, don't re-read them in the repl function.
           ;; (which was the old way).
           (sbcli::repl)))

      (error (c)
        (format! *error-output* "Unexpected error: ~a~&" c)
        (return-from top-level/handler)))
    ))


;; ZSH completion.
(defun top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (zsh-completion/command)))

(defun zsh-completion/command ()
  "Returns a command for generating the Zsh completion script.
  Installation instructions are given in the output."
  (clingon:make-command
   :name "zsh-completion"
   :description "generate the Zsh completion script"
   :usage ""
   :handler (lambda (cmd)
              ;; Use the parent command when generating the completions,
              ;; so that we can traverse all sub-commands in the tree.
              (let ((parent (clingon:command-parent cmd)))
                (clingon:print-documentation :zsh-completions parent t)))))

(defun main ()
  "Entry point for the binary. Parse options."
  (let ((app (top-level/command)))
    (clingon:run app)))

(defun starts-with-verbose-option (args)
  (member (first args) (list "-v" "--verbose") :test #'equal))

(defmethod clingon:parse-command-line :around ((command clingon:command) arguments)
  "Calls parse-command-line, but treats all unknown options as free arguments.
  Our goal is to pass unknown options to the script, without using a \"--\" to separate the CIEL options from the script's options.

  This works, but it handles all unknown options, including those coming first (that should error out legitimely), and Clingon handles known options coming last, after the script, as valid for CIEL.
  Ultimately, we would like this:

  $ ciel -s script -x 42 -v

  to give options -x, 42 AND -v to the script."
  ;; (uiop:format! t "-- all cli args: ~s~&" (uiop:command-line-arguments))
  (handler-bind ((clingon:unknown-option
                   (lambda (c)
                     ;; The command is not yet parsed. Did we get a --verbose though?
                     (when (starts-with-verbose-option (uiop:command-line-arguments))
                       (uiop:format! *error-output* "[ciel] cli parsing: passing ~a to script~&" (clingon:unknown-option-name c)))
                     (clingon:treat-as-argument c))))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; top-level for binary construction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;
(format t "~&Registering built-in scripts in src/scripts/ …~&")
(register-builtin-scripts)
