

#|
Currently, usage is to "use" ciel along with cl:

(defpackage mypackage
  (:use :cl :ciel))

How we build this package
=========================

A note on symbol conflicts.

We like very much Alexandria, Serapeum and Generic-cl. However we
can't "use" them all at once, some symbols conflict. And some are very
different things, especially with generic-cl. For example:
SERAPEUM:HASH-TABLE-SET and GENERIC-CL:HASH-TABLE-SET.

We think that providing the user with Serapeum's symbol on CIEL-USER
on one hand, and with Generic-cl's one on GENERIC-CIEL on the other
hand is the thing not to do. So we don't import these symbols by
default in CIEL. They are present in GENERIC-CIEL (since we "use"
generic-cl).

|#


(in-package :ciel)

(defparameter *doc-pages* '()
  "We want to track the symbols that we import from other libraries, and re-display their documentation automatically on their own page.

We currently only try this with serapeum. See *deps/serapeum/sequences-hashtables* and how the docs/serapeum.md page is generated with `generate-dependencies-page-reference'.")

;; UIOP
(cl-reexport:reexport-from :uiop
                           :include
                           '(:define-package ;; shall we override defpackage?
                             :find-symbol*
                             :format!
                             :println
                             :symbol-call
                             :not-implemented-error
                             :command-line-arguments))

;; Syntax.
(cl-reexport:reexport-from :pythonic-string-reader
                           :include
                           '(:enable-pythonic-string-syntax
                             :disable-pythonic-string-syntax))

;; Pattern matching.
(cl-reexport:reexport-from :trivia
                           :include
                           '(:match
                             :guard))

;; "let" with more destructuring.
(cl-reexport:reexport-from :metabang-bind
                           :include
                           '(:bind))

(cl-reexport:reexport-from :parse-float)
(cl-reexport:reexport-from :parse-number
                           :include
                           '(:parse-number
                             :parse-positive-real-number
                             :parse-real-number))

(cl-reexport:reexport-from :access
                           :include '(:access
                                      :accesses))

(cl-reexport:reexport-from :arrow-macros
                           ;XXX: they would need docstrings.
                           :include '(:->
                                      :->>
                                      :some->
                                      :some->>
                                      :as->
                                      :cond->
                                      :cond->>
                                      :-<>
                                      :-<>>
                                      :some-<>
                                      :some-<>>))

;XXX: it's only in the latest Quicklisp
(cl-reexport:reexport-from :trivial-do
                           :include
                           '(:doalist ;; key value alist
                             :dohash  ;; key value ht
                             :dolist* ;; position value list
                             ;; :doplist ;; already from alexandria
                             ;; :doseq   ;; value sequence  ;; conflicts with generic-cl
                             ;; :doseq*  ;; position value sequence
                             ))

;; alexandria/sequences/lists
(defparameter *deps/alexandria/sequences-lists*
  '(:iota
    :proper-list
    :proper-list-p
    :proper-sequence
    :circular-list
    :circular-list-p
    :doplist
    :ensure-cons
    :ensure-list
    :flatten
    :setp

    ;; :emptyp ;; conflicts with generic-cl
    :shuffle
    :random-elt
    :length=
    :last-elt
    ))

(cl-reexport:reexport-from :alexandria
                           :include
                           *deps/alexandria/sequences-lists*)

;; alexandria-2/sequences
;; That one is quite well hidden, it is not even in the manual. Why so?
(defparameter *deps/alexandria-2/sequences*
  '(:subseq*))

(cl-reexport:reexport-from :alexandria
                           :include
                           *deps/alexandria-2/sequences*)

;; alexandria/hash-tables
(defparameter *deps/alexandria/hash-tables*
  '(:hash-table-keys
    :hash-table-values
    :hash-table-alist
    :ensure-gethash))
(cl-reexport:reexport-from :alexandria
                           :include
                           *deps/alexandria/hash-tables*)

;; alexandria/numbers
(cl-reexport:reexport-from :alexandria
                           :include
                           '(:mean :variance :median
                             :clamp))

(push (list "docs/alexandria.md"
            :alexandria
            (union
             *deps/alexandria/sequences-lists*
             *deps/alexandria/hash-tables*)
            "Symbols imported from ALEXANDRIA for sequences and hash-tables")
      *doc-pages*)

;; alexandria/flow
(defparameter *deps/alexandria/flow*
  '(:if-let
    :when-let
    :when-let*
    ))
(cl-reexport:reexport-from :alexandria
                           :include *deps/alexandria/flow*)
(push (list "docs/alexandria-control-flow.md"
            :alexandria
            *deps/alexandria/flow*
            "Symbols imported from ALEXANDRIA for control flow.")
      *doc-pages*)

;; serapeum: sequences and hash tables
(defparameter *deps/serapeum/sequences-hashtables*
  '(:assort
    :batches
    :filter
    :iota
    :runs
    :partition
    :partitions
    :split-sequence
    :frequencies

    :count-cpus

    ;; hash-tables
    :dict
    :do-hash-table ;; see also trivial-do
    :dict*
    :dictq ;; quoted
    :pophash
    :swaphash
    :hash-fold
    :maphash-return
    :merge-tables
    :flip-hash-table
    :set-hash-table
    ;; :hash-table-set ;; conflicts with generic-cl. They are different things.
    :hash-table-predicate
    :hash-table-function
    :make-hash-table-function
    :delete-from-hash-table
    :pairhash
    ;; to be continued
    ))

(push (list "docs/serapeum.md"
            :serapeum
            *deps/serapeum/sequences-hashtables*
            "Symbols imported from SERAPEUM for sequences and hashtables")
      *doc-pages*)

(cl-reexport:reexport-from :serapeum
                           :include
                           *deps/serapeum/sequences-hashtables*)


;; serapeum: conditions and type helpers.
(cl-reexport:reexport-from :serapeum
                           :include
                           '(:ignoring
                             ;; Compile-time exhaustiveness checking
                             :ecase-of
                             :etypecase-of
                             :ctypecase-of
                             :typecase-of
                             :case-of
                             :ccase-of))

;; serapeum: definitions
(cl-reexport:reexport-from :serapeum
                           :include
                           '(:defalias))

;; serapeum: functions
(cl-reexport:reexport-from :serapeum
                           :include
                           '(:partial
                             :juxt))
;;
(cl-reexport:reexport-from :alexandria
                           :include
                           '(:rcurry))


(cl-reexport:reexport-from :trivial-arguments
                           :include '(:arglist))

(defparameter *deps/trivial-types*
  '(
    :association-list-p
    :type-expand
    :string-designator
    :property-list
    ;; :proper-list ;; in alexandria
    ;; :proper-list-p ;; in alexandria
    :association-list
    :character-designator
    :property-list-p
    :file-associated-stream-p
    :type-specifier-p
    :list-designator
    :package-designator
    :non-nil
    :file-associated-stream
    :stream-designator
    :function-designator
    :file-position-designator
    :pathname-designator
    ))

(cl-reexport:reexport-from :trivial-types
                           :include *deps/trivial-types*)
(push (list "docs/trivial-types.md"
            :trivial-types
            *deps/trivial-types*)
      *doc-pages*)


(cl-reexport:reexport-from :defstar
                           :include '(:defun*
                                      :defmethod*
                                      :defgeneric*
                                      :defparameter*
                                      :defvar*))

;; ppcre
(serapeum:defalias apropos-regex #'ppcre:regex-apropos)
(serapeum:defalias apropos-regex-list #'ppcre:regex-apropos-list)

(export '(apropos-regex
          apropos-regex-list))

;; function-cache: memoization
(cl-reexport:reexport-from :function-cache
                           :include '(:defcached))  ;; not in binary??


;;;
;;; Conveniently add type declarations.
;;; Straight from Serapeum, only it is -> thus it conflicts with our arrow-macro.
;;;
(deftype --> (args values)
  "The type of a function from ARGS to VALUES.

  From SERAPEUM (where it is -> and thus conflicts with our -> arrow-macro)."
  `(function ,args ,values))

(defmacro --> (function args values)
  "Declaim the ftype of FUNCTION from ARGS to VALUES.

     (--> mod-fixnum+ (fixnum fixnum) fixnum)
     (defun mod-fixnum+ (x y) ...)

  In pure CL, it would be:

  (declaim (ftype (function (fixnum fixnum) fixnum) mod-fixnum+))
  (defun mod-fixnum+ (x y) ...)

  In CIEL, you can also use `defun*'.

  From SERAPEUM (where it is -> and thus conflicts with our -> arrow-macro)."
  `(declaim (ftype (--> ,args ,values) ,function)))


;; from Bard.
(defmacro ^ (&rest forms)
  "^ is a synonym macro for lambda.

(^ (x) (+ x 10))
=>
(lambda (x) (+ x 10))"
  `(lambda ,@forms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-documentation (symbol &key (stream t))
  "Print the available documentation for this symbol."
  ;; Normally, the documentation function takes as second argument the
  ;; type designator. We loop over each type and print the available
  ;; documentation.
  ;; XXX: copied and adapted from repl.lisp :S
  ;; - print arglist first, inside backquotes
  ;; - enforce newlines
  ;; - doc -> docstring for conflict with repl-utilities
  (handler-case (loop for doc-type in '(variable function structure type setf)
                   with sym = (if (stringp symbol)
                                  ;; used from the readline REPL
                                  (read-from-string symbol)
                                  ;; used from Slime
                                  symbol)
                   for docstring = (unless (consp sym) ;; when a function is quoted: :doc 'defun
                                     ;; instead of :doc defun
                                     (documentation sym doc-type))
                   when (and (equal doc-type 'function)
                             (fboundp sym))
                   do (format stream "~%ARGLIST: `~a`~%"
                              (format nil "~(~a~)"
                                      (trivial-arguments:arglist sym)))
                   when  docstring
                   do (format stream "~%~a: ~a~&" doc-type docstring))
    (error (c) (format *error-output* "Error during documentation lookup: ~a~&" c))))

(defun generate-dependencies-page-reference ()
  (loop for doc-spec in (reverse *doc-pages*)
     ;; reverse used during development. If we push the same data onto *doc-pages*, the previous one is still processed after the one, so we won't see changes in a doc/….md file.
     ;; We could erase the first occurence.
     do
       (format t "~&Formating ~a doc to ~a: ~a~&" (second doc-spec) (first doc-spec) (third doc-spec))
       (with-open-file (f (first doc-spec)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
         (if (fourth doc-spec)
             (format f "# ~a~&~%" (fourth doc-spec))
             (format f "# Symbols imported from ~a~&~%" (second doc-spec)))
         (loop for elt in (third doc-spec)
            for sym = (uiop:find-symbol* elt (second doc-spec))
            do
              (format f "~%## ~a ~%~a~&"
                      elt
                      (with-output-to-string (s)
                        (symbol-documentation sym :stream s)))))))

;;
;; Generate documentation.
;;
#+only-with-a-C-c-C-c
(generate-dependencies-page-reference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ciel-user)

(defvar *script-args* (list)
  "A list of command line arguments that are targetting the script called by ciel -s script")

;; We would like triple quotes for the functions docstring by default,
;; but this conflicts with other packages using a reader macro on the double quote
;; by using cl-syntax (Jonathan, Djula).
;; (pythonic-string-reader:enable-pythonic-string-syntax)

;; Limit the maximum default output.
(setf *print-lines* 1000)
(setf *print-level* 20)
(setf *print-length* 1000)

;; Pretty-print hash-tables by default.
;; Enable/disable with toggle-pretty-print-hash-table
;; (adapted from rutils)
;; PR accepted upstream, November 2020. Pretty-printing is only off by default.

(defparameter *pretty-print-hash-tables* t "Pretty-print hash tables by default.")
(defparameter *current-pprint-indentation* 1
  "We use custom indentation instead of the pretty printer, because it doesn't print correctly in the shell (indentations are way too large).")

;; TEST
(defun pretty-print-hash-table (ht &optional (stream *standard-output*))
  "Pretty print hash-table HT to STREAM.

  Adapted from RUTILS." ;; and ported to Serapeum.
  (let ((*print-pretty* t)
        (i 0))
    (pprint-logical-block (stream nil)
      (format stream "~&")
      (format stream "~vt" *current-pprint-indentation*)
      (princ "(dict " stream)
      (unless (eq (hash-table-test ht) 'equal)
        (princ #\' stream)
        (princ (hash-table-test ht) stream))
      (incf *current-pprint-indentation*)
      (format stream "~vt" *current-pprint-indentation*)
      (block nil
        (maphash (lambda (k v)
                   (format stream "~&")
                   (when (and *print-length* (> (incf i) *print-length*))
                     (princ "..." stream)
                     (return))
                   (when (and k (listp k))
                     (princ #\' stream))
                   (if (typep k 'hash-table)
                       (pretty-print-hash-table k stream)
                       (format stream "~vt~s" *current-pprint-indentation* k))
                   (princ " " stream)
                   (when (and v (listp v))
                     (princ #\' stream))
                   (if (typep v 'hash-table)
                       (pretty-print-hash-table v stream)
                       (format stream "~s" v)))
                 ht))
      (decf *current-pprint-indentation*)
      (format stream "~vt" *current-pprint-indentation*)
      (format stream "~&")
      (format stream "~vt) " *current-pprint-indentation*)))
  ht)

(let (toggled)
  (defun toggle-pretty-print-hash-table (&optional (on nil explicit))
    "Toggles printing hash-tables with PRINT-HASH-TABLE or with default method.
     If ON is set explicitly will turn on literal printing (T) or default (NIL).

     CIEL note: this function comes from RUTILS (which is not installed by default)."
    ;XXX: merged in Serapeum November, 2020
    (let ((off (if explicit on (not toggled))))
      (if off
          (progn
            (set-pprint-dispatch 'hash-table (serapeum:flip #'pretty-print-hash-table))
            (setf toggled t))
          (progn
            (set-pprint-dispatch 'hash-table nil)
            (setf toggled nil))))))

(when *pretty-print-hash-tables*
  (toggle-pretty-print-hash-table t))

(defun ciel-user-help ()
  "Print a short welcome and help message."
  (format t "CIEL version ~a~&" (asdf:system-version (asdf:find-system "ciel")))
  (format t "Documentation: https://ciel-lang.github.io/CIEL/~&"))
