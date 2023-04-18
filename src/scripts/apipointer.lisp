#!/usr/bin/env ciel
;;;
;;; Call a JSON API, access elements with json-pointer.
;;;
;;; Example:
;;;
;;; $ ciel -s apipointer https://fakestoreapi.com/products?limit=5 "/0/rating/rate"
;;; or
;;; $ ./apipointer.lisp etc etc
;;;

(in-package :ciel-user)

;; Currently, only -> is imported from arrow-macros. Use more?
(import 'arrow-macros:-<>)
(import 'arrow-macros:<>)

;;; For the example sake, we don't do error handling in (main),
;;; because when the script is run, CIEL adds a handler-case to handle
;;; any error and print a short message, sans the stacktrace.
;;; Proper error handling is left as an exercise to the reader.

(defun main (url pointer)
  (-<> url
    dex:get
    json:read-json
    (json-pointer:get <> pointer)
    ;; for a terminal output:
    pprint))

(unless (second uiop:*command-line-arguments*)
  (format! t "APIPointer: request a JSON API, get nested elements with json-pointer.~&")
  (format! t "Usage: ciel apipointer.lisp URL [JSON-POINTERS]~&")
  (uiop:quit))

;; Feature flag:
;; call our main function only when running the script, not when developing on the REPL.
#+ciel
(main (second uiop:*command-line-arguments*) (third uiop:*command-line-arguments*))
#-ciel
(format t "Usage: (main url pointer)~&")
