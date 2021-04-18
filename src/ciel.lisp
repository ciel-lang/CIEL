
(in-package :cl-user)
(defpackage ciel
  (:use :cl)
  (:export :enable-shell-passthrough))

(in-package :ciel)

(defparameter *doc-pages* '()
  "We want to track the symbols that we import from other libraries, and re-display their documentation automatically on their own page.

We currently only try this with serapeum. See *deps/serapeum/sequences-hashtables* and how the docs/serapeum.md page is generated with `generate-dependencies-page-reference'.")

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
                             :doseq   ;; value sequence
                             :doseq*  ;; position value sequence
                             ))

(cl-reexport:reexport-from :repl-utilities
                           :include
                           '(:repl-utilities
                             :readme
                             :doc
                             :summary
                             :package-apropos
                             :trace-package
                             :print-hash))

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

    :emptyp
    :shuffle
    :random-elt
    :length=
    :last-elt
    ))

(cl-reexport:reexport-from :alexandria
                           :include
                           *deps/alexandria/sequences-lists*)
;; alexandria/numbers
(cl-reexport:reexport-from :alexandria
                           :include
                           '(:mean :variance :median
                             :clamp))

(push (list "docs/alexandria.md"
            :alexandria
            *deps/alexandria/sequences-lists*
            "Symbols imported from ALEXANDRIA for sequences and lists")
      *doc-pages*)

;; serapeum: sequences/hash tables
(defparameter *deps/serapeum/sequences-hashtables*
  '(:assort
    :batches
    :iota
    :runs
    :partition
    :partitions
    :split-sequence

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
    :hash-table-set
    :hash-table-predicate
    :hash-table-function
    :make-hash-table-function
    :delete-from-hash-table
    :pairhash
    ;; to be continued
    ))

(push (list "docs/serapeum.md"
            :serapeum
            *deps/serapeum/sequences-hashtables*)
      *doc-pages*)

(cl-reexport:reexport-from :serapeum
                           :include
                           *deps/serapeum/sequences-hashtables*)


;; serapeum: conditions and type helpers.
(cl-reexport:reexport-from :serapeum
                           :include
                           '(:ignoring
                             ;; Compile-time exhaustiveness checking
                             :etypecase-of
                             :ctypecase-of
                             :typecase-of
                             :case-of
                             :ccase-of))

;; serapeum: definitions
(cl-reexport:reexport-from :serapeum
                           :include
                           '(:defalias))

(cl-reexport:reexport-from :trivial-arguments
                           :include '(:arglist))

(defparameter *deps/trivial-types*
  '(
    :association-list-p
    :type-expand
    :string-designator
    :property-list
    :tuple
    ;; :proper-list ;; in alexandria
    :association-list
    :character-designator
    :property-list-p
    :file-associated-stream-p
    :type-specifier-p
    :list-designator
    :package-designator
    ;; :proper-list-p ;; in alexandria
    :tuplep
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
  (loop for doc-spec in *doc-pages*
     do
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
              (format f "## ~a ~%~%~a~&"
                      elt
                      (with-output-to-string (s)
                        (symbol-documentation sym :stream s)))))))

#+only-with-a-C-c-C-c
(generate-dependencies-page-reference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage ciel-user
  (:use :cl :ciel)
  (:local-nicknames (:csv :cl-csv)
                    (:http :dexador)))

;TODO: a conflict between Serapeum and generic-cl
;; (defpackage generic-ciel
  ;; (:use :generic-cl
        ;; :ciel))

(in-package :ciel-user)

;; Enable triple quotes for the functions docstring.
;; (in-readtable pythonic-string-reader:pythonic-string-syntax)
(pythonic-string-reader:enable-pythonic-string-syntax)

;; cl-json wants to convert our lisp symbols to camelCase, and the JSON ones to lisp-case.
;; We disable that.
(setf json:*json-identifier-name-to-lisp* #'identity)
(setf json:*lisp-identifier-name-to-json* #'identity)

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

(defun enable-shell-passthrough ()
  "Enable the shell passthrough with \"!\". Enable Clesh's readtable."
  (named-readtables:in-readtable clesh:syntax))
