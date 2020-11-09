;; #!/usr/bin/sbcl --script
(load "~/quicklisp/setup")

(let ((*standard-output* (make-broadcast-stream)))
  ;; (progn (load "ciel.asd") (ql:quickload '("swank" "ciel")))
  (ql:quickload '("alexandria" "which" "trivial-package-local-nicknames"))
  (ql:quickload "cl-readline"))

(defpackage :sbcli
  (:use :common-lisp :cffi :trivial-package-local-nicknames)
  (:export sbcli help what *repl-version* *repl-name* *prompt* *prompt2* *ret* *config-file*
           *hist-file* *special* *last-result*
           *syntax-highlighting* *pygmentize* *pygmentize-options*))

(in-package :sbcli)

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
(defvar *repl-name*    "CIEL's REPL for SBCL")
(defvar *prompt*       (format nil "~a" (cl-ansi-text:green "ciel-user> ")))
(defvar *prompt2*       "....> ")
(defvar *ret*          "=> ")
(defvar *config-file*  "~/.cielrc")
(defvar *hist-file*    "~/.ciel_history")
(defvar *last-result*  nil)
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
  (format stream "!! Quicklisp is not installed !!"))

(defun read-hist-file ()
  (with-open-file (in *hist-file* :if-does-not-exist :create)
    (loop for line = (read-line in nil nil)
      while line
      ; hack because cl-readline has no function for this. sorry. ;INFO: it does now.
      do (cffi:foreign-funcall "add_history"
                               :string line
                               :void))))

(defun update-hist-file (str)
  (with-open-file (out *hist-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (write-line str out)))

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

(defun add-res (txt res)
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
    (lambda (k v) (format t "  :~a => ~a~%" k (documentation (cdr v) t)))
    *special*)
  ;; (write-line "Currently defined:")
  ;; (print-currently-defined)
  (write-line "Press CTRL-D or type :q to exit"))

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

(defun toggle-lisp-critic ()
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
     )
   :test 'equal)
  "All special commands starting with :")

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
  (let* ((splt (str:words text))
         (k (subseq (car splt) 1 (length (car splt))))
         (v (gethash k *special*)))
    (if v
      (call-special v (car splt) (cdr splt))
      (format *error-output* "Unknown special command: ~a~%" k))))

(defun evaluate-lisp (text parsed)
  (setf *last-result*
        (handler-case (eval parsed)
          (unbound-variable (var) (format *error-output* "~a~%" var))
          (undefined-function (fun) (format *error-output* "~a~%" fun))
          (sb-int:compiled-program-error ()
            (format *error-output* "~a"
                    (cl-ansi-text:red "Compiler error.~%")))
          (error (condition)
            (format *error-output* "~a~a~%"
                    (cl-ansi-text:red "Evaluation error: ")
                    condition))))
  (add-res text *last-result*)
  (if *last-result*
      (format t "~a~s~%" *ret* *last-result*)))

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
           (str:starts-with-p ":" text))
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

#+or(nil)
(progn
  (assert (member "str:concat"
                  (select-completions "str:con" (list "str:containsp" "str:concat" "str:constant-case"))
                  :test #'string-equal)))

(defun custom-complete (text &optional start end)
  "Custom completer function for readline, triggered when we press TAB.

  START and END are required in the lambda list but are not used. We
  only complete package and function names, they would help to
  complete arguments."
  (declare (ignore start end))
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
       ((zerop (length pkg-name))
        (list-symbols-and-packages sym-name))
       (external-p
        (list-external-symbols sym-name pkg-name))
       (t
        (list-internal-symbols sym-name pkg-name))))))

#+or(nil)
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
      ((str:ends-with-p " ?" text)
       (sbcli::symbol-documentation (last-nested-expr text)))
      (t
       (sbcli::handle-input txt text)))
    (finish-output nil)
    (format t "~&")
    (sbcli::sbcli "" *prompt*)))

(defun repl ()
  (rl:register-function :complete #'custom-complete)
  (rl:register-function :redisplay #'syntax-hl)

  (if (probe-file *config-file*)
      (load *config-file*))

  (princ *banner*)
  (write-line (str:repeat 80 "-"))
  (print-system-info)
  (write-line (str:repeat 80 "-"))
  (help)
  (write-char #\linefeed)
  (finish-output nil)

  (when *hist-file* (read-hist-file))

  (in-package :ciel-user)
  (handler-case (sbcli::sbcli "" sbcli::*prompt*)
    (sb-sys:interactive-interrupt () (sbcli::end))))

;; When trying it out with --script:
;; (repl)
