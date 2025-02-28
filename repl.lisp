;; #!/usr/bin/sbcl --script
(ignore-errors (load "~/quicklisp/setup"))
#+quicklisp
(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "cl-readline"))
;; update <2024-09-04>: now all shell commands are run interactively.
;;; It works for htop, vim, sudo, emacs -nw…
;;;
;;; update <2025-02-03>: the "!" "pass-through" is disabled on Slime and "dumb" terminals.

(uiop:define-package :sbcli
    (:use :common-lisp :trivial-package-local-nicknames)
  (:import-from :magic-ed
   :magic-ed)
  (:export repl sbcli help what *repl-version* *repl-name* *prompt* *prompt2* *result-indicator* *init-file*
           *quicklisp*
           *hist-file* *special*
           *syntax-highlighting* *pygmentize* *pygmentize-options*))

(in-package :sbcli)

;; repl-utilities: nice to have, but don't clutter the CIEL package by exporting them.
;; For instance, "summary" is too common a word to be exported.
(cl-reexport:reexport-from :repl-utilities
                           :include
                           '(:repl-utilities
                             :readme
                             ;; :doc  ;; conflicts with our little %doc helper.
                             :summary
                             :package-apropos
                             :trace-package
                             :print-hash))

(defvar *repl-version* "0.1.5") ;XXX: print CIEL version.
(defvar *banner* "

       _..._
    .-'_..._''.                         .---.
  .' .'      '.\.--.      __.....__     |   |
 / .'           |__|  .-''         '.   |   |
. '             .--. /     .-''''-.  `. |   |
| |             |  |/     /________\   \|   |
| |             |  ||                  ||   |
. '             |  |\    .-------------'|   |
 \ '.          .|  | \    '-.____...---.|   |
  '. `._____.-'/|__|  `.             .' |   |
    `-.______ /         `''-...... -'   '---'
             `


")
(defvar *repl-name*    "CIEL's REPL")
(defvar *prompt*       (format nil "~a" (cl-ansi-text:green "ciel-user> ")))
(defvar *prompt2*       "....> ")
(defvar *result-indicator*          "=> ")
(defvar *init-file*  "~/.cielrc")
(defvar *hist-file*    "~/.ciel_history")
(defvar *hist*         (list))
(defvar *syntax-highlighting* nil)
(defvar *pygmentize* nil "(optional) Path to a pygments executable. If not set, we try to find it.")
(defvar *pygmentize-options* (list "-s" "-l" "lisp"))
(defparameter *lisp-critic* nil "If non-nil, give feedback on the code you type using lisp-critic.")
(declaim (special *special*))

(defun print-system-info (&optional (stream t))
  ;; see also https://github.com/40ants/cl-info
  (format stream "~&OS: ~a ~a~&" (software-type) (software-version))
  (format stream "~&Lisp: ~a ~a~&" (lisp-implementation-type) (lisp-implementation-version))
  #+asdf
  (format stream "~&ASDF: ~a~&" (asdf:asdf-version))
  #-asdf
  (format stream "NO ASDF!")
  #+quicklisp
  (format stream "~&Quicklisp: ~a~&" (ql-dist:all-dists))
  #-quicklisp
  (format stream "Quicklisp is not installed~&"))

(defun read-hist-file ()
  (with-open-file (in *hist-file* :if-does-not-exist :create)
    (loop for line = (read-line in nil nil)
      while line
      ;; hack because cl-readline has no function for this.
      ;; TODO: it has it now.
      do (cffi:foreign-funcall "add_history"
                               :string line
                               :void))))

(defun update-hist-file (str)
  (with-open-file (out *hist-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (write-line str out)))

(defun load-init-file (&optional (init-file *init-file*))
  "Load the ~/.cielrc init file.
  Defaults to `*init-file*'."
  (load init-file))

(defun end ()
  "Ends the session."
  (format t "~%Bye!~&")
  (uiop:quit))

;; (defun reset ()
;;   "Resets the session environment"
;;   (delete-package 'sbcli)
;;   (defpackage :sbcli (:use :common-lisp :ciel))
;;   ;XXX: ?
;;   (in-package :sbcli))

(defun novelty-check (str1 str2)
  (string/= (string-trim " " str1)
            (string-trim " " str2)))

(defun history-add (txt res)
  (setq *hist* (cons (list txt res) *hist*)))

(defun format-output (&rest args)
  (format (car args) "~a ; => ~a" (caadr args) (cadadr args)))

(defun write-to-file (fname)
  "Writes the current session to a file <filename>"
  (with-open-file (file fname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format file "~{~/sbcli:format-output/~^~%~}" (reverse *hist*))))

(defun what (sym)
  "Gets help on a symbol <sym>: :? str"
  (format t "inspecting ~a.~&~
  To inspect further objects, type their number.~&~
  To quit, type q and Enter." sym)
  (handler-case (inspect (read-from-string sym))
    (error (c) (format *error-output* "Error during inspection: ~a~%" c))))

(defun help ()
  "Prints this general help message"
  (format t "~a version ~a~%" *repl-name* *repl-version*)
  (write-line "Read more on packages with readme or summary. For example: (summary :str)")
  (write-line "Special commands:")
  (maphash
    (lambda (k v) (format t "  %~a => ~a~%" k (documentation (cdr v) t)))
    *special*)
  ;; (write-line "Currently defined:")
  ;; (print-currently-defined)
  (write-line "Press CTRL-D or type %q to exit"))

(defun symbol-documentation (symbol)
  "Print the available documentation for this symbol."
  ;; Normally, the documentation function takes as second argument the
  ;; type designator. We loop over each type and print the available
  ;; documentation.
  (handler-case (loop for doc-type in '(variable function structure type setf)
                   with sym = (if (stringp symbol)
                                  ;; used from the readline REPL
                                  (read-from-string symbol)
                                  ;; used from Slime
                                  symbol)
                   for doc = (unless (consp sym) ;; when a function is quoted: :doc 'defun
                               ;; instead of :doc defun
                               (documentation sym doc-type))
                   when  doc
                   do (format t "~a: ~a~&" doc-type doc)
                   when (and (equal doc-type 'function)
                             (fboundp sym))
                   do (format t "ARGLIST: ~a~&" (format nil "~(~a~)"
                                                        (trivial-arguments:arglist sym))))
    (error (c) (format *error-output* "Error during documentation lookup: ~a~&" c))))

(defun print-currently-defined ()
  (do-all-symbols (s *package*)
    (when (and (or (fboundp s) (boundp s)) (eql (symbol-package s) *package*))
      (let ((what (cond ((fboundp s) 'function) ((constantp s) 'constant) (t 'variable))))
        (format t " ~a: ~a (~a) ~a~%" (string-downcase (string s))
                                      (or (documentation s what)
                                          "No documentation")
                                      what
                                      (if (boundp s)
                                        (format nil "(value ~a)" (eval s))
                                        ""))))))

(defun dump-disasm (sym)
  "Dumps the disassembly of a symbol <sym>"
  (handler-case (disassemble (read-from-string sym))
    (unbound-variable (var) (format t "~a~%" var))
    (type-error (err) (format t "~a~%" err))
    (sb-int:compiled-program-error (err) (format t "~a~%" err))
    (undefined-function (fun) (format t "~a~%" fun))))

(defun dump-type (expr)
  "Prints the type of a expression <expr>"
  (handler-case (format t "~a~%" (type-of (eval (read-from-string expr))))
    (unbound-variable (var) (format t "~a~%" var))
    (type-error (err) (format t "~a~%" err))
    (sb-int:compiled-program-error (err) (format t "~a~%" err))
    (undefined-function (fun) (format t "~a~%" fun))))

(defun edit-and-load-file (file)
  "Edit a file with EDITOR and evaluate it."
  (magic-ed file))

(defun toggle-lisp-critic ()
  "Enable or disable the lisp critic. He critizes the code you type before compiling it."
  (setf *lisp-critic* (not *lisp-critic*))
  (format t "The lisp-critic is ~a.~&" (if *lisp-critic* "enabled" "disabled")))

;; -1 means take the string as one arg
(defvar *special*
  (alexandria:alist-hash-table
   `( ;; ("help" . (0 . ,#'general-help))
     ("help" . (0 . ,#'help))
     ("doc" . (1 . ,#'symbol-documentation))
     ("?" . (1 . ,#'what))
     ;; ("r" . (1 . ,#'readme))
     ;; ("s" . (1 . ,#'summary))
     ("w" . (1 . ,#'write-to-file))
     ("d" . (1 . ,#'dump-disasm))
     ("t" . (-1 . ,#'dump-type))
     ("q" . (0 . ,#'end))
     ;; ("z" . (0 . ,#'reset))
     ("lisp-critic" . (0 . ,#'toggle-lisp-critic))
     ("edit" . (1 . ,#'edit-and-load-file))
     )
   :test 'equal)
  "All special commands starting with :")

(defun special-command-p (text)
  "A *special* command starts with %."
  (str:starts-with-p "%" text))

;; both functions are required to get completion on %
(defun list-special-commands ()
  (loop for k being the hash-key of *special*
     collect (format nil "%~a" k)))
(defun intern-special-commands ()
  (loop for k being the hash-key of *special*
     for symname = (format nil "%~a" k)
     do (intern symname :sbcli)))
(intern-special-commands)

(defun call-special (fundef call args)
  (let ((l (car fundef))
        (fun (cdr fundef))
        (rl (length args)))
    (cond
      ((= -1 l) (funcall fun (str:join " " args)))
      ((< rl l)
        (format *error-output*
                "Expected ~a arguments to ~a, but got ~a!~%"
                l call rl))
      (t (apply fun (subseq args 0 l))))))

(defun handle-special-input (text)
  (let* ((words (str:words text))
         (k (subseq (car words) 1 (length (car words))))
         (v (gethash k *special*)))
    (if v
      (call-special v (car words) (cdr words))
      (format *error-output* "Unknown special command: ~a~%" k))))

(defun evaluate-lisp (text parsed)
  "Evaluate (EVAL) the user input.
  In case of evaluation error, print it.
  Then print the result. Print its multiple values.
  Save the input history.
  Handle the special *, + et all REPL history variables."
  (let ((result-list
         (multiple-value-list
          (handler-case (eval parsed)
            (unbound-variable (var)
              (format *error-output* "~a~%" var))
            (undefined-function (fun)
              (format *error-output* "~a~%" fun))
            (sb-int:compiled-program-error ()
              (format *error-output* "~a"
                      (cl-ansi-text:red "Compiler error.~%")))
            (error (condition)
              (format *error-output* "~a~a~%"
                      (cl-ansi-text:red "Evaluation error: ")
                      condition))))))
    (history-add text (car result-list))
    (when result-list
      (setf +++ ++
            /// //
            *** (car ///)
            ++ +
            // /
            ** (car //)
            + parsed
            / result-list
            * (car result-list))
      ;; Print the result, and all multple values. They are printed like so:
      ;; (not the best with =>)
      ;; ciel-user> (values 1 2 3)
      ;; => 1
      ;; 2
      ;; 3
      (format t "~a~{~s~&~}~%" *result-indicator* result-list))))

#+(or nil)
(let* ((input "(values :one :two)")
       (result (with-output-to-string (*standard-output*)
                 (evaluate-lisp "whatever" (read-from-string input)))))
  (assert (and (str:containsp ":one"
                              result
                              :ignore-case t)
               (str:containsp ":two"
                              result
                              :ignore-case t)))
  (assert (equal '(:ONE :TWO)
                 /)))


(defun lisp-critic-applicable (txt)
  "TXT is code that should start with a parenthesis. Don't critique global variables."
  (str:starts-with? "(" (str:trim txt)))

(defun handle-lisp (before text)
  (let* ((new-txt (format nil "~a ~a" before text))
         (parsed (handler-case (read-from-string new-txt)
                   (end-of-file () (sbcli new-txt *prompt2*))
                   (error (condition)
                     (format *error-output* "Parser error: ~a~%" condition))))
         (to-critic (when (and *lisp-critic*
                               (lisp-critic-applicable new-txt)
                               parsed)
                      `(lisp-critic:critique ,parsed))))

    (when to-critic
      ;; The call to lisp-critic doesn't evaluate the lisp code,
      ;; it only scans it and prints feedback.
      (evaluate-lisp text to-critic))
    ;; But even if the lisp-critic is enabled,
    ;; we want the code we type to be eval'ed.
    (when parsed
      (evaluate-lisp text parsed))))

(defun handle-input (before text)
  (if (and (> (length text) 1)
           (special-command-p text))
    (handle-special-input text)
    (handle-lisp before text)))

(defun get-package-for-search (text)
  "Return a list with:
  - the text after the colon or double colon
  - the package name
  - T if we look for an external symbol, NIL for an internal one."
  (let ((pos))
    (cond
      ((setf pos (search "::" text))
       (list (subseq text  (+ pos 2))
             (subseq text 0 pos)
             nil))
      ((setf pos (position #\: text))
       (if (zerop pos)
           (list text nil t)
           (list (subseq text (1+ pos))
                 (subseq text 0 pos)
                 t)))
      (t (list text nil  t)))))

(defun list-external-symbols (sym-name pkg-name)
  "List external symbols of PKG-NAME (a string).
  (the symbol name is currently ignored)."
  (declare (ignorable sym-name))
  (assert (stringp pkg-name))
  (loop :for sym :being :the :external-symbols :of (find-package pkg-name)
     :collect (format nil "~(~a:~a~)" pkg-name sym)))

(defun list-internal-symbols (sym-name pkg-name)
  "List internal symbols of the package named PKG-NAME (a string)."
  (declare (ignorable sym-name))
  (assert (stringp pkg-name))
  (loop :for sym :being :the :symbols :of (find-package pkg-name)
     :collect (format nil "~(~a::~a~)" pkg-name sym)))

(defun list-local-nicknames (&optional (package *package*))
  "Return a list of local nicknames.
 (downcased strings, with a trailing colon to denote a package)"
  (loop :for pair in (package-local-nicknames package)
     :collect (format nil "~a:" (str:downcase (car pair)))))

(defun list-symbols-and-packages (sym-name)
  "Base case, when the user entered a string with no colon that would delimit a package.
  Return the current packages, symbols of the current package, current keywords.
  They are filtered afterwards, in SELECT-COMPLETIONS."
  (declare (ignorable sym-name))
  (concatenate 'list
               (list-special-commands)
               (loop :for pkg :in (list-all-packages)
                  :append (loop :for name :in (package-nicknames pkg)
                             :collect (format nil "~(~a:~)" name))
                  :collect (format nil "~(~a:~)" (package-name pkg)))
               (list-local-nicknames *package*)
               (loop :for sym :being :the :symbols :of *package*
                  :collect (string-downcase sym))
               (loop :for kw :being :the :symbols :of (find-package "KEYWORD")
                  :collect (format nil ":~(~a~)" kw))))

(defun select-completions (text items)
  "TEXT is the string entered at the prompt, ITEMS is a list of
strings to match candidates against (for example in the form \"package:sym\")."
  (setf items
        (loop :for item :in items
           :when (str:starts-with-p text item)
           :collect item))
  (unless (cdr items)
    (setf rl:*completion-append-character*
          (if (str:ends-with-p ":" (car items))
              #\nul
              #\space))
    (return-from select-completions items))
  (cons
   (subseq (car items) 0
           (loop :for item :in (cdr items)
              :minimize (or (mismatch (car items) item) (length item))))
   items))

#+(or)
(progn
  (assert (member "str:concat"
                  (select-completions "str:con" (list "str:containsp" "str:concat" "str:constant-case"))
                  :test #'string-equal)))

(defun shell-passthrough-p (s)
  "Return t if s (string) starts with \"!\".
   We also use it to offer custom TAB completion."
  (str:starts-with-p "!" s))

(defun complete-filename-p (text start end &key (line-buffer rl:*line-buffer*))
  "Return T if we should feed the tab completion candidates filenames, instead of the regular Lisp symbols.
  We answer yes when we are tab-completing a secord word on the prompt and a quote comes before it.

  TEXT, START and END: see `custom-complete'.

 Ex:

  !ls \"test TAB   => yes return files instead of lisp symbols for completion.
  !\"tes TAB       => well, no.
  (load \"test TAB => yes
  (load (test TAB  => no
"
  (declare (ignore end))
  (and (not (shell-passthrough-p text))
       (> start 1)  ;; 1 is an opening parenthesis.
       (char-equal #\" (elt line-buffer (1- start))) ;; after an opening quote.
       ))

#+test-ciel
(progn
  (assert (complete-filename-p "test" 7 10 :line-buffer "(load \"test"))
  (assert (complete-filename-p "test" 7 10 :line-buffer "(!foo \"test"))
  (assert (not (complete-filename-p "test" 1 5 :line-buffer "\"test")))
  )

(defun filter-candidates (text file-candidates)
  "Return a list of files (strings) in the current directory that start with TEXT."
  ;; yeah, this calls for more features. Hold on a minute will you.
  (remove-if #'null
             (mapcar (lambda (path)
                       (let ((namestring (file-namestring path)))
                         (when (str:starts-with-p text namestring)
                           namestring)))
                     file-candidates)))

(defun complete-binaries-from-path-p (text start end &key (line-buffer rl:*line-buffer*))
  "Return T if we should TAB-complete shell executables, and search them on the PATH.

  START must be 0: we are writing the first word on the readline prompt,
  TEXT must start with ! the mark of the shell pass-through."
  (declare (ignore end line-buffer))
  (and (zerop start)
       (str:starts-with-p "!" text)))

(defun find-binaries-candidates (text)
  "Find binaries starting with TEXT in PATH.

  Return: a list of strings."
  (loop with s = (string-left-trim "!" text)
        for dir in (uiop:getenv-absolute-directories "PATH")
        for res = (filter-candidates s (uiop:directory-files dir))
        collect res into candidates
        finally (return
                  ;; we got "!text", we have to return candidates
                  ;; with the "!" prefix, so that readline agrees they are completions.
                  (mapcar (lambda (bin)
                            (str:concat "!" bin))
                          (alexandria:flatten candidates)))))

(defun custom-complete (text &optional start end)
  "Custom completer function for readline, triggered when we press TAB.

  Complete filenames on the current directory when appropriate (after a quote).

  TEXT is the current word being type. Not the full command line.

  START is the start of this word. If we type the first word of the command
  and TAB-complete it, then START equals 0. For a second word, START != 0.

  Ex:

   !ls te TAB

  TEXT is \"te\", START is 4 and END is 6.

  That way we give other completion candidates depending on START."
  (when (string-equal text "")
    (return-from custom-complete nil))
  (destructuring-bind (sym-name pkg-name external-p)
      (get-package-for-search (string-upcase text))
    (when (and pkg-name
               (not (find-package pkg-name)))
      (return-from custom-complete nil))

    (select-completions
     (str:downcase text)
     (cond
       ((complete-binaries-from-path-p text start end :line-buffer rl:*line-buffer*)
        (find-binaries-candidates text))
       ((complete-filename-p text start end :line-buffer rl:*line-buffer*)
        ;; complete file names on the current directory.
        ;; Yes we could complete both: lisp symbols AND files. See with usage.
        (filter-candidates text (uiop:directory-files ".")))
       ((zerop (length pkg-name))
        (list-symbols-and-packages sym-name))
       (external-p
        (list-external-symbols sym-name pkg-name))
       (t
        (list-internal-symbols sym-name pkg-name))))))

#+(or)
(progn
  (assert (member "str:suffixp"
                  (custom-complete "str:suff")
                  :test #'string-equal))
  (assert (member "uiop:file-exists-p"
                  (custom-complete "uiop:file-")
                  :test #'string-equal)))

(defun format-prompt (text &key (colored t))
  (let ((prompt (str:concat text "> ")))
    (format nil "~a" (if colored
                         (cl-ansi-text:green prompt)
                         prompt))))

(defun run-visual-command (text)
  "Run this visual command (string, sans \"!\" prefix)."
  (if (termp:termp)
      (uiop:run-program (string-left-trim "!" text)
                        :output :interactive
                        :input :interactive)
      (uiop:format! *error-output* "~&Cannot run this shell command: we are not inside a \"real\" terminal.~&")))

(defun sbcli (txt prompt)
  "Read user input and evaluate it.
  This function must be called from inside the CIEL-USER package."
  (let* ((prompt-text (if (functionp prompt)
                          (funcall prompt)
                          prompt))
         (cur-pkg-name (package-name *package*))
         (text
          (handler-case
              (rl:readline :prompt (if (string-equal "CIEL-USER" cur-pkg-name)
                                       prompt-text
                                       (sbcli::format-prompt cur-pkg-name))
                           :add-history t
                           :novelty-check #'sbcli::novelty-check)
            ;; Catch a C-c.
            (#+sbcl sb-sys:interactive-interrupt
              #+ccl  ccl:interrupt-signal-condition
              #+clisp system::simple-interrupt-condition
              #+ecl ext:interactive-interrupt
              #+allegro excl:interrupt-signal
              ()
              (write-char #\linefeed)
              ""))))

    (unless text (sbcli::end))
    (if (string= text "")
        (sbcli::sbcli "" *prompt*))
    (when *hist-file* (sbcli::update-hist-file text))
    (cond
      ;; Handle documentation lookup.
      ((str:ends-with-p " ?" text)
       (sbcli::symbol-documentation (last-nested-expr text)))

      ;; Interactive and visual shell command?
      ;; All shell commands are run interactively.
      ((shell-passthrough-p text)
       (run-visual-command text))

      ;; Default: run the lisp command (with the lisp-critic, the shell passthrough
      ;; and other add-ons).
      (t
       (sbcli::handle-input txt text)))
    (finish-output nil)
    (format t "~&")
    (sbcli::sbcli "" *prompt*)))

(defun edit-current-input (arg key)
  ;; experimental, doesn't properly work.
  (declare (ignore arg key))
  (let ((filename "/tmp/ciel-temp.lisp")
        (current-input rl:*line-buffer*))
    (str:to-file filename current-input)
    (magic-ed filename)
    ;; ... user writes...
    ;; (NB: rl:replace-line preserves the point position and that's annoying)
    ;; (setf rl:*line-buffer* (str:trim (str:from-file filename)))
    ;; (rl:redisplay)
    ;; (rl:delete-text 0 rl:+end+)
    (uiop:format! t "text is: ~a~&" (str:from-file filename))
    ;; (rl:insert-text (str:concat "hello" (str:trim (str:from-file filename))))
    (setf rl:*line-buffer* (str:trim (str:from-file filename)))
    (rl:redisplay)
    ))

(defun repl (&key noinform no-usernit)
  "Toplevel REPL.

  CLI options:
  - -h, --help
  - --noinform: don't print the welcome banner.
  - --no-userinit: don't load the user's cielrc init file.
  "

  (let ((argv (uiop:command-line-arguments)))
    (when (or (member "-h" argv :test #'string-equal)
              (member "--help" argv :test #'string-equal))
      (format t "~a version ~a~%" *repl-name* *repl-version*)
      (format t "Contribute on: https://github.com/ciel-lang/CIEL~&")
      (print-system-info)
      (format t "CIEL Is an Extended Lisp. It's Common Lisp, batteries included.~&~
        It comes in the form of a Quicklisp library that you can use as any other one in your favourite editor, ~
        as an SBCL core image and as a readline REPL, with developer goodies.~&")
      (uiop:quit)))

  (rl:register-function :complete #'custom-complete)
  (rl:register-function :redisplay #'syntax-hl)

  ;; testing…
  (defun print-some-text (arg key)
    (declare (ignore arg key))
    (rl:insert-text "inserted text"))

  #+(or)
  (rl:bind-keyseq "\\C-o" #'print-some-text)
  (rl:bind-keyseq "\\C-x\\C-e" #'edit-current-input)
  (rl:set-paren-blink-timeout 500)

  ;; Print a banner and system info.
  ;; Checking a CLI arg this way is an old, done before our use of Clingon.
  (unless (or noinform
              (member "--noinform" (uiop:command-line-arguments) :test #'string-equal))
    (princ *banner*)
    (write-line (str:repeat 80 "-"))
    (print-system-info)
    (write-line (str:repeat 80 "-"))
    (help)
    (write-char #\linefeed)
    (finish-output nil))

  ;; Load CIEL's user init file.
  (unless (or no-usernit
              (member "--no-userinit" (uiop:command-line-arguments) :test #'string-equal))
    (when (uiop:file-exists-p *init-file*)
      (load-init-file)))

  (when *hist-file* (read-hist-file))

  (in-package :ciel-user)

  (handler-case (sbcli::sbcli "" sbcli::*prompt*)
    (error (c)
      ;; Normally lisp code is evaled and protected from errors in evaluate-lisp.
      ;; We need this for magic-ed.
      ;; As a special command it doesn't use evaluate-lisp.
      (format *error-output* "~&Error: ~a~&" c)
      (sbcli::sbcli "" sbcli::*prompt*))
    (sb-sys:interactive-interrupt ()
      (sbcli::end))))

;; When trying it out with --script:
;; (repl)
