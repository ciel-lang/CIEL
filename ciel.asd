#|
  This file is a part of ciel project.
|#

(require "asdf")  ;; for CI

(asdf:defsystem "ciel"
  :description "CIEL Is an Extended Lisp (Common Lisp, batteries included)."
  :version "0.1"
  :author "vindarel"
  :license "MIT"
  :homepage "https://github.com/ciel-lang/CIEL/"
  :source-control (:git "https://github.com/ciel-lang/CIEL/")
  :bug-tracker "https://github.com/ciel-lang/CIEL/issues/"

  :depends-on (
               :cl-reexport ;; for us
               :cl-ansi-text

               :access
               :alexandria
               :arrow-macros
               :fof  ;; file-object-finder

               ;; threads
               :bordeaux-threads
               :trivial-monitored-thread
               :moira
               :lparallel
               :cl-cron

               :closer-mop
               :cl-ansi-text
               :cl-csv
               :shasht
               :dissect
               :fset
               :generic-cl

               ;; web
               :dexador
               :hunchentoot
               :easy-routes ;; better route definition for Hunchentoot.
               :quri
               :lquery

               ;; GUI
               :ltk

               :local-time
               :modf

               ;; number parsing
               :parse-float
               :parse-number

               ;; database
               :dbi  ;; connects and executes queries.
               :sxql ;; SQL generator from lispy syntax.

               ;; numerical
               :vgplot

               ;; regexp
               :cl-ppcre

               ;; string manipulation
               :str

               ;;;
               ;;; Language extensions.
               ;;;
               ;; triple quotes
               :pythonic-string-reader

               ;; pattern matching
               :trivia
               :trivial-arguments
               :trivial-package-local-nicknames
               :trivial-types

               ;; extended let
               :metabang-bind

               ;; type declarations
               :defstar

               ;; iteration
               :for
               :trivial-do

               ;; lambda shorthands
               :fn
               :cl-punch

               :cmd
               :serapeum
               :shlex

               ;; tests
               :fiveam

               :which

               ;;;
               ;;; Debugging, developer utilities.
               ;;;
               :log4cl
               :printv
               :repl-utilities ;; see readme, summary, doc, package-apropos, trace-package etc

               ;;;
               ;;; User helpers.
               ;;; ;TODO: we don't want these dependencies when we build a binary.
               ;;;
               :named-readtables
               :clesh  ;; shell pass-through
               :quicksearch  ;; search on GitHub, Cliki, Quickdocs.
               )
  :components ((:module "src"
                        :components
                        ((:file "packages")
                         (:file "ciel")))
               (:module "src/more-docstrings"
                        :components
                        ((:file "docstrings"))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sub-system for the terminal REPL.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(asdf:defsystem "ciel/repl"
  :description "readline REPL for CIEL with quality of life improvements."
  :depends-on (;; :ciel  ;; let's avoid, it could run side effects twice (like a defparameter set then reset).
               ;; deps
               :cl-readline
               :cffi  ;; "tmp", for cl-readline add history
               :lisp-critic  ;; it would be nice to integrate it with Slime.
               :magic-ed)
  :components ((:file "repl")
               (:file "shell-utils")
               (:file "repl-utils"))

  ;; Build a binary with Deploy, ship foreign libraries (and ignore libssl).
  :defsystem-depends-on (:deploy)  ;; need to (ql:quickload "deploy") before building.
  ;; :build-operation "program-op"
  :build-operation "deploy-op"
  :build-pathname "ciel-repl"
  :entry-point "sbcli::repl")

;; Don't ship libssl, rely on the target OS'.
;; Needs to require or quickload cl+ssl before we can compile and load this .asd file? :S
#+linux (deploy:define-library cl+ssl::libssl :dont-deploy T)
#+linux (deploy:define-library cl+ssl::libcrypto :dont-deploy T)

;; Use compression: from 114M, 0.02s startup time to 27M and 0.42s (SBCL 2.0.10).
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

;; Even with the binary, ASDF wants to update itself and crashes
;; if it doesn't find an ASDF directory, like on a user's system.
;; Thanks again to Shinmera.
(deploy:define-hook (:deploy asdf) (directory)
  (declare (ignorable directory))
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () nil))
