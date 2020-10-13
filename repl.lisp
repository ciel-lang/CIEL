;; #!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup")

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "alexandria")
  (ql:quickload "cl-readline"))

(defpackage :sbcli
  (:use :common-lisp :cffi)
  (:export sbcli help what *repl-version* *repl-name* *prompt* *prompt2* *ret* *config-file*
           *hist-file* *special* *last-result*))

(defpackage :cieli-user                 ;; note "i"
  (:use :cl :ciel :sbcli))

;; (in-package :sbcli)
(in-package :cieli-user)

(defvar *repl-version* "0.1.3")
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
(defvar *repl-name*    "CIEL's REPL for SBCL
based on SBCLI")
(defvar *prompt*       (format nil "~a" (cl-ansi-text:green "ciel-user> ")))
(defvar *prompt2*       "....> ")
(defvar *ret*          "=> ")
(defvar *config-file*  "~/.cielrc")
(defvar *hist-file*    "~/.ciel_history")
(defvar *last-result*  nil)
(defvar *hist*         (list))
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

(defun reset ()
  "Resets the session environment"
  (delete-package 'sbcli)
  (defpackage :sbcli (:use :common-lisp :ciel))
  (in-package :sbcli))

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
  (write-line "Press CTRL-C or CTRL-D or type :q to exit"))

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

(defun common-prefix (items)
  (let ((lst 0))
    (loop for n from 1 below (reduce #'min (mapcar #'length items)) do
      (when (every (lambda (x)
                      (char= (char (car items) n)
                             (char x           n)))
              (cdr items))
       (setf lst n)))
   (write lst)
   (subseq (car items) 0 (1+ lst))))

(defun starts-with (text)
  (lambda (sym)
    (let* ((symstr (string-downcase sym))
           (cmp (subseq symstr 0 (min (length symstr) (length text)))))
      (string= text cmp))))

(defun select-completions (text list)
 (let* ((els (remove-if-not (starts-with text)
                           (mapcar #'string list)))
        (els (if (cdr els) (cons (common-prefix els) els) els)))
    (if (string= text (string-downcase text))
      (mapcar #'string-downcase els)
      els)))

(defun get-all-symbols ()
  (let ((lst ()))
    (do-all-symbols (s lst)
      (when (or (fboundp s) (boundp s)) (push s lst)))
    lst))

(defun custom-complete (text start end)
  (declare (ignore start) (ignore end))
  (select-completions text (get-all-symbols)))


;; -1 means take the string as one arg
(defvar *special*
  (alexandria:alist-hash-table
   `(;; ("help" . (0 . ,#'general-help))
     ("help" . (0 . ,#'help))
     ("?" . (1 . ,#'what))
     ;; ("r" . (1 . ,#'readme))
     ;; ("s" . (1 . ,#'summary))
     ("w" . (1 . ,#'write-to-file))
     ("d" . (1 . ,#'dump-disasm))
     ("t" . (-1 . ,#'dump-type))
     ("q" . (0 . ,#'end))
     ("z" . (0 . ,#'reset)))
   :test 'equal))

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
            (format *error-output* "Compiler error.~%"))
          (error (condition)
            (format *error-output* "Evaluation error: ~a~%" condition))))
  (add-res text *last-result*)
  (if *last-result* (format t "~a~a~%" *ret* *last-result*)))

(defun handle-lisp (before text)
  (let* ((new-txt (format nil "~a ~a" before text))
         (parsed (handler-case (read-from-string new-txt)
                   (end-of-file () (sbcli new-txt *prompt2*))
                   (error (condition)
                    (format *error-output* "Parser error: ~a~%" condition)))))
    (when parsed (evaluate-lisp text parsed))))

(defun handle-input (before text)
  (if (and (> (length text) 1) (string= (subseq text 0 1) ":"))
    (handle-special-input text)
    (handle-lisp before text)))

(defun sbcli (txt p)
  (let ((text
          (rl:readline :prompt (if (functionp p) (funcall p) p)
                       :add-history t
                       :novelty-check #'novelty-check)))
    (in-package :cieli-user)
    (unless text (end))
    (if (string= text "") (sbcli "" *prompt*))
    (when *hist-file* (update-hist-file text))
    (handle-input txt text)
    (in-package :sbcli)
    (finish-output nil)
    (sbcli "" *prompt*)))

(defun repl ()
  ;; TODO: I don't have completion.
  (rl:register-function :complete #'custom-complete)

  (if (probe-file *config-file*)
      (load *config-file*))

  (print *banner*)
  (print "hello !!!")
  (write-line (str:repeat 80 "-"))
  (print-system-info)
  (write-line (str:repeat 80 "-"))
  (help)
  (write-char #\linefeed)
  (finish-output nil)

  (when *hist-file* (read-hist-file))

  (handler-case (sbcli "" *prompt*)
    (sb-sys:interactive-interrupt () (end))))
