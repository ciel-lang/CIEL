
(in-package :cl-user)

(ql:quickload "cl+ssl")  ;; only because of Deploy's parameters.
;; (asdf:load-asd "./ciel.asd")
;; Bug on CI, needs an absolute pathname.
(let ((pathname (merge-pathnames "ciel.asd" (uiop:getcwd))))
  (uiop:format! t "~&--- loading this asd absolute pathname: ~S~&" pathname)
  (asdf:load-asd pathname))

(ql:quickload "swank")
(ql:quickload "ciel")

(in-package :ciel-user)

(sb-ext:save-lisp-and-die "ciel-core")
