#!/usr/bin/env ciel
;;;
;;; Call a JSON API, access elements with json-pointer.
;;;
;;; Example:
;;;
;;; $ ciel -s apipointer https://fakestoreapi.com/products?limit=5 "/0/rating/rate"
;;;

(in-package :ciel-user)

;; Currently, only -> is imported from arrow-macros. Use more?
(import 'arrow-macros:-<>)
(import 'arrow-macros:<>)

(defun main (url pointer)
  (handler-case
      (-<> url
        dex:get
        json:read-json
        (json-pointer:get-by <> pointer)
        ;; for a terminal output:
        pprint)
    (error (c)
      (format *error-output* "An error occured: ~a~&" c))))

(unless (second uiop:*command-line-arguments*)
  (format! t "APIPointer: request a JSON API, get nested elements with json-pointer.~&")
  (format! t "Usage: ciel apipointer.lisp URL [JSON-POINTERS]~&")
  (uiop:quit))

(main (second uiop:*command-line-arguments*) (third uiop:*command-line-arguments*))
