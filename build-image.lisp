
(in-package :cl-user)

(asdf:load-asd "ciel.asd")
(ql:quickload "swank")
(ql:quickload "ciel")

(in-package :ciel-user)

(sb-ext:save-lisp-and-die "ciel")
