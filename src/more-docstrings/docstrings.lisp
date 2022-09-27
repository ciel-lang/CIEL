(uiop:define-package :more-docstrings
    (:use :cl))

(in-package :more-docstrings)

#|
Add more documentation and add examples to the docstrings.

Now, when we read a function documentation from our editor or with the
built-in `documentation` function, we can learn more about it, and see
examples. We have less the need to reach for external resources.

The goal is still to ease the first contact of newcomers with CL.
For example, give examples on how to use MAP, MAPCAR, MAPCAN.

XXX: gotchas

- if we quickload :ciel twice, the docstrings
are appended twice too :S The hash-table cache doesn't help in that
case.
- we are modifying the symbols in the :cl package, not the ones in :ciel.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; One function to do the job:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (str:concat doc s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now use it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; mapcar
(docstring-append 'mapcar "

For example:

(mapcar #'+ '(1 2 3) '(10 20 30))  ;; => (11 22 33)

(mapcar (lambda (x)
          (format t \"~a is ~R~&\" x x))
        '(1 2 3))
;; =>
1 is one
2 is two
3 is three
(NIL NIL NIL)
")

;;; mapcan
(docstring-append 'mapcan "

NCONC concatenates lists destructively.")

;;; sort
(docstring-append 'sort "

Since SORT is destructive, use COPY-LIST:

(setq mylist (list 1 3 2))
(sort (copy-list mylist) #'<)

See also STABLE-SORT.")

(docstring-append 'loop "
The basic LOOP structure is

(loop for x in (list x y z)
   do …)

\"do\" is for side effects.

Use \"collect\" to return results:

(loop for x in (list 1 2 3)
  collect (* x 10))

To iterate over arrays, use \"across\" instead of \"in\".

To iterate over hash-tables… try MAPHASH first :D

For many examples, see the CL Cookbook:
https://lispcookbook.github.io/cl-cookbook/iteration.html")

(docstring-append 'maphash "

Example:

(maphash (lambda (key value)
           (format t \"key is: ~a, value is: ~a~&\" key value))
         (dict :a 'one))
;; => key is: A, value is: ONE")

;;; to be continued.
