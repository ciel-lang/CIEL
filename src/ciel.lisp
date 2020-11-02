(in-package :cl-user)
(defpackage ciel
  (:use :cl))

(in-package :ciel)

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
(cl-reexport:reexport-from :alexandria
                           :include
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
;; alexandria/numbers
(cl-reexport:reexport-from :alexandria
                           :include
                           '(:mean :variance :median
                             :clamp))

(cl-reexport:reexport-from :serapeum
                           :include
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
                             :dictq  ;; quoted
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

;; Conditions and type helpers.
(cl-reexport:reexport-from :serapeum
                           :include
                           '(:ignoring
                             ;; Compile-time exhaustiveness checking
                             :etypecase-of
                             :ctypecase-of
                             :typecase-of
                             :case-of
                             :ccase-of))

(cl-reexport:reexport-from :trivial-arguments
                           :include '(:arglist))

(cl-reexport:reexport-from :trivial-types
                           :include '(
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

(defpackage ciel-user
  (:use :cl :ciel)
  (:local-nicknames (:match :trivia)
                    (:csv :cl-csv)
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

;TODO: pretty print hash-tables

;; Pretty-print hash-tables by default.
;; Enable/disable with toggle-print-hash-table
;; (stolen from rutils)

(defparameter *pretty-print-hash-tables* t "Pretty-print hash tables by default.")

(defparameter *current-pprint-indentation* 1)

(defun print-hash-table (ht &optional (stream *standard-output*))
  "Pretty print hash-table HT to STREAM.

  CIEL note: copied from RUTILS and adapted to print correctly in the terminal."
  ;; We use *current-pprint-indentation* instead of the built-in pprint-indent and friends
  ;; because printing in the terminal prints too many tabs and too many lines in-between elements.
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
                       (print-hash-table k stream)
                       (format stream "~vt~s" *current-pprint-indentation* k))
                   (princ " " stream)
                   (when (and v (listp v))
                     (princ #\' stream))
                   (if (typep v 'hash-table)
                       (print-hash-table v stream)
                       (format stream "~s" v)))
                 ht))
      (decf *current-pprint-indentation*)
      (format stream "~vt" *current-pprint-indentation*)
      (format stream "~&")
      (format stream "~vt) " *current-pprint-indentation*)))
  ht)

;; ;; Or:
;; (defmethod print-hash-table ((object hash-table) stream)
;;   ;; XXX: we can not read this back.
;;   (format stream "#HASH{~a, ~{~{~a: ~a~}~^, ~}}"
;;           (hash-table-test object)
;;           (loop for key being the hash-keys of object
;;              using (hash-value value)
;;              collect (list key value))))

(let ((default-method (ignore-errors (find-method
                                      #'print-object nil '(hash-table t))))
      toggled)
  (defun toggle-print-hash-table (&optional (on nil explicit))
    "Toggles printing hash-tables with PRINT-HASH-TABLE or with default method.
     If ON is set explicitly will turn on literal printing (T) or default (NIL).

     CIEL note: this function comes from RUTILS (which is not installed by default)."
    (let ((off (if explicit on (not toggled))))
      (if off
          (progn
            (defmethod print-object ((obj hash-table) stream)
              (print-hash-table obj stream))
            (setf toggled t))
          (progn (remove-method #'print-object
                                (find-method #'print-object nil '(hash-table t)))
                 (unless (null default-method)
                   (add-method #'print-object default-method))
                 (setf toggled nil))))))

(when *pretty-print-hash-tables*
  (toggle-print-hash-table t))
