#|
  This file is a part of ciel project.
|#

(asdf:defsystem "ciel"
  :version "0.1"
  :author "vindarel"
  :license "MIT"
  :depends-on (
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
               :dexador
               :dissect
               :fset
               :generic-cl

               ;; web
               :hunchentoot
               :easy-routes  ;; better route definition for Hunchentoot.
               :djula
               :spinneret
               :quri

               :local-time
               :modf

               ;; number parsing
               :decimals
               :parse-float
               :parse-number

               :cl-ppcre
               :pythonic-string-reader
               :str
               :trivia  ;; pattern matching
               :trivial-arguments
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

               ;; debugging, developer utilities
               :log4cl
               :printv
               :repl-utilities  ;; see readme, summary, doc, package-apropos, trace-package etc
               )
  :components ((:module "src"
                :components
                ((:file "ciel"))))

  :description "CIEL Is an Extended Lisp.")
