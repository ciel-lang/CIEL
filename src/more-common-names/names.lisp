
(uiop:define-package :more-common-names
    (:use :cl)
  (:export :nappend
           :nremove))

(in-package :more-common-names)

;; from cl-str
(defun concat (&rest strings)
  "Join all the string arguments into one string."
  (apply #'concatenate 'string strings))

;; from more-docstrings.
(defvar *docstrings-cache* (make-hash-table)
  "Cache the original docstring of functions and macros we are augmenting.
  Mainly to ease our tests at the REPL.")

(defun documentation-with-cache (symbol &optional (doc-type 'function))
  (let ((cached (gethash symbol *docstrings-cache*)))
    (if cached
        cached

        (let ((doc (documentation symbol doc-type)))
          (setf (gethash symbol *docstrings-cache*)
                ;; don't store a NIL docstring.
                (or doc ""))
          doc))))

(defun docstring-append (symbol s &optional (doc-type 'function))
  "Add S to the docstring of SYMBOL (to designate a function or a macro).
  DOC-TYPE is the required argument of DOCUMENTATION, by default 'function (for functions and macros), otherwise use 'variable."
  (let ((doc (documentation-with-cache symbol doc-type)))
    (setf (documentation symbol doc-type)
          (concat doc s))))


;; nappend
(setf (fdefinition 'nappend) #'nconc)

(docstring-append 'nappend "

An alias to the built-in NCONC."
                  'function)

;; nremove
(setf (fdefinition 'nremove) #'delete)

(docstring-append 'nremove "
Return a sequence formed by destructively removing the specified ITEM from
the given SEQUENCE.

An alias to the built-in DELETE."
                  'function)
