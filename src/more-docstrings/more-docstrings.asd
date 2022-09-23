(in-package :cl-user)

;; This .asd definition is not used in CIEL, where docstring.lisp is
;; included in the sources, so we just load the .lisp file.
;;
;; It is there to give it a chance to be used outside of CIEL. We'll see.
(asdf:defsystem "more-docstrings"
  :version "0.1"
  :author "vindarel"
  :license "MIT"
  :homepage "https://github.com/ciel-lang/more-docstrings/"
  :depends-on ("str")
  :components ((:file "docstrings"))

  :description "more-docstrings augments the docstring of built-in functions, macros and variables to give more explanations and examples. It is part of the CIEL project.")
