#!/usr/bin/env ciel
;;;
;;; Search for Lisp libraries on Quicklisp, Cliki and Github.
;;;
;;;
;;; Run with:
;;; $ ciel -s quicksearch keyword
;;;
;;; or add a shebang line and make this script executable.
;;;

(in-package :ciel-user)

(unless (second uiop:*command-line-arguments*)
  (format! t "Quicksearch: search for Lisp libraries on Quicklisp, Cliki and Github.~&")
  (format! t "Usage: ciel quicksearch.lisp keyword~&")
  (uiop:quit))

;; We use a "feature flag" kind of like a "file == __main__" check:
;; don't run this when developing on the REPL, but yes run it when calling it with CIEL.
#+ciel
(quicksearch:? (second uiop:*command-line-arguments*) :ud)  ;; url and details.
