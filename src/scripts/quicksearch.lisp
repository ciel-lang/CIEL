;;;
;;; Search for Lisp libraries on Quicklisp, Cliki and Github.
;;;
;;;
;;; Run with:
;;; $ ciel quicksearch keyword
;;;
;;; or add a shebang line and make this script executable.
;;;

(in-package :ciel-user)

(unless (second (uiop:command-line-arguments))
  (format! t "Quicksearch: search for Lisp libraries on Quicklisp, Cliki and Github.~&")
  (format! t "Usage: ciel quicksearch.lisp keyword~&")
  (uiop:quit))

(quicksearch:? (second (uiop:command-line-arguments)) :ud)  ;; url and details.
