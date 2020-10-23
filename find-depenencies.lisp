#!/usr/bin/sbcl --script

#|
Show CIEL's dependencies.

Redirect the script output to doc/dependencies.md
|#

(require 'asdf)
(load "~/quicklisp/setup")
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
  (let ((systems (mapcar #'asdf/find-system:find-system
                         (system-dependencies deps/str))))
    (loop for system in systems
         do (format t "- ~a: ~a~&" (asdf:primary-system-name system)
                    (str:shorten 200 (asdf:system-description system))))))

(print-dependencies "ciel")
