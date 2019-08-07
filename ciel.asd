#|
  This file is a part of ciel project.
|#

(asdf:defsystem "ciel"
  :version "0.1"
  :author "vindarel"
  :license "MIT"
  :depends-on (
               :alexandria
               :arrow-macros
               :bordeaux-threads
               :closer-mop
               :cl-annot
               :cl-ansi-text
               :cl-csv
               :cl-interpol
               :cl-json
               :cl-yaclyaml            ;; only loader, not writer.
               :decimals
               :defclass-std
               :dexador
               :dissect
               :fset
               :generic-cl
               :listopia  ;; list manipulation library, inspired by Haskell.
               :hunchentoot
               :local-time
               :log4cl
               :modf
               :parse-float
               :parse-number
               :cl-ppcre
               :pythonic-string-reader
               :quri
               :str
               :trivia  ;; pattern matching
               :trivial-arguments
               ;; :trivial-monitored-thread ;; newer quicklisp
               :unix-opts

               ;; iteration
               :iterate
               :for

               ;; lambda shorthands
               :fn
               :cl-punch

               ;; database
               :mito
               :sxql
               )
  :components ((:module "src"
                :components
                ((:file "ciel"))))

  :description "A bigger CL, batteries included.")
