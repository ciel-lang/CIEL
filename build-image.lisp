
(in-package :cl-user)

(ql:quickload "deploy")  ;; not used to build the image, but used in the .asd.
(ql:quickload "cl+ssl")  ;; only because of Deploy's parameters.
;; (asdf:load-asd "./ciel.asd")
;; Bug on CI, needs an absolute pathname.
(let ((pathname (merge-pathnames "ciel.asd" (uiop:getcwd))))
  (uiop:format! t "~&--- loading this asd absolute pathname: ~S~&" pathname)
  (asdf:load-asd pathname))

(ql:quickload "swank")
(ql:quickload "ciel")

(in-package :ciel-user)

;; XXX: we would like to read our ~/.cielrc init file when resuming the core
;; in Slime.
;; Currently we load it only when starting the terminal REPL.
;; See the :toplevel option.

(sb-ext:save-lisp-and-die "ciel-core")
