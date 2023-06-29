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
               :shasht  ;; json
               :cl-json-pointer/synonyms
               :dissect
               :fset
               :file-notify  ;; needs inotify (linux) or fsevent (macos)
               :generic-cl

               ;; web
               :dexador
               :hunchentoot
               :easy-routes ;; better route definition for Hunchentoot.
               :quri
               :lquery
               :spinneret   ;; lispy templates. Used in simpleHTTPserver.lisp

               ;; GUI
               :nodgui  ;; ltk fork with built-in themes and more widgets.

               ;; CLI
               :clingon  ;; args parsing

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
                         (:file "json-pointer-minus")
                         (:file "ciel")
                         (:file "gui")))
               (:file "utils")
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
               :lisp-critic  ;; it would be nice to integrate it with Slime.
               :magic-ed)
  :components ((:file "repl")
               (:file "utils")
               (:file "scripting")
               (:file "shell-utils")
               (:file "repl-utils")

               ;; I define them here, for good practice (for me),
               ;; but I don't use them.
               ;; static-file is important, otherwise the scripts would be run.
               (:module "src/scripts"
                        :components
                        ((:static-file "quicksearch")
                         (:static-file "simpleHTTPserver")))
               )

  ;; Build a binary with Deploy, ship foreign libraries (and ignore libssl).
  :defsystem-depends-on (:deploy)  ;; need to (ql:quickload "deploy") before building.
  ;; :build-operation "program-op"
  :build-operation "deploy-op"
  :build-pathname "ciel"
  :entry-point "ciel::main")

;;; This defines ciel.asd. It is enough to quickload CIEL.
;;; But to build a binary,
;;; see build-config.lisp for extra config.
