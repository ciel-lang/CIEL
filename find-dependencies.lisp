#!/usr/bin/sbcl --script

#|
Show CIEL's dependencies.

Run as a script.

Redirect the script output to docs/dependencies.md (see Makefile).
|#

(require 'asdf)
(print "loading quicklisp…")
(load "~/quicklisp/setup")

(require "cl+ssl")
(load "ciel.asd")
(require 'swank) ;; but why?
(ql:quickload '("ciel" "str") :silent t)

(defun system-dependencies (system/str)
  "Return a list of system names, as strings."
  (sort
   (asdf:system-depends-on (asdf/find-system:find-system system/str))
   #'string<
   :key #'asdf/system:primary-system-name))

;; where's a project URL?

(defun print-dependencies (deps/str)
  ;XXX: doesn't find dependencies from package-inferred-systems (like fof).
  (let ((systems (mapcar #'asdf/find-system:find-system
                         (system-dependencies deps/str))))
    (loop for system in systems
         do (format t "- ~a: ~a~&" (asdf:primary-system-name system)
                    (str:shorten 200 (asdf:system-description system))))))

(format t "<!-- list generated automatically. -->~&")
(print-dependencies "ciel")
