#|
  This file is a part of ciel project.
|#

(asdf:defsystem "ciel"
  :version "0.1"
  :author "vindarel"
  :license "MIT"
  :depends-on (
               :cl-reexport ;; for us
               :cl-readline    ;; for the binary
               :cl-ansi-text

               :access
               :alexandria
               :arrow-macros

               ;; threads
               :bordeaux-threads
               :trivial-monitored-thread
               :moira
               :lparallel
               :cl-cron

               :closer-mop
               :cl-ansi-text
               :cl-csv
               :cl-json
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

               :cl-ppcre
               :pythonic-string-reader
               :str
               :trivia ;; pattern matching
               :trivial-arguments
               :trivial-package-local-nicknames
               :trivial-types

               ;; iteration
               :iterate
               :for
               :trivial-do

               ;; lambda shorthands
               ;; xxx: or rutils
               :fn
               :cl-punch

               ;; database
               :mito
               :sxql

               ;; :rutils ;; yes?
               :serapeum

               ;; tests
               :fiveam

               :which

               ;; debugging, developer utilities
               :log4cl
               :printv
               :repl-utilities ;; see readme, summary, doc, package-apropos, trace-package etc
               )
  :components ((:module "src"
                        :components
                        ((:file "ciel")
                         ))
               (:file "repl")
               (:file "repl-utils"))

  :build-operation "program-op"
  :build-pathname "ciel-repl"
  :entry-point "sbcli::repl"

  :description "CIEL Is an Extended Lisp.")
