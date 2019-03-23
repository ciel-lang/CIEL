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
               :defclass-std
               :dexador
               :dissect
               :fset
               ;; :generic-cl ;; on newer quicklisp
               ;; :listopia ;; not until reddit's review is fixed.
               :log4cl
               :modf
               :parse-float
               :cl-ppcre
               :pythonic-string-reader
               :str
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
