(in-package :cl-user)
(defpackage ciel
  (:use :cl)
  (:import-from :parse-float
                :parse-float)
  (:import-from :access
                :access
                :accesses)
  (:import-from :trivial-do
                :doalist ;; key value alist
                :dohash  ;; key value ht
                :dolist* ;; position value list
                :doplist ;; key value plist
                :doseq   ;; value sequence
                :doseq*  ;; position value sequence
                )
  (:import-from :repl-utilities
                :readme
                :doc
                :summary
                :package-apropos
                :trace-package
                :print-hash)
  (:import-from :serapeum
                :assort
                :batches
                :iota
                :runs
                :partition
                :partitions
                :split-sequence

                ;; Compile-time exhaustiveness checking
                :etypecase-of
                :ctypecase-of
                :typecase-of
                :case-of
                :ccase-of

                :count-cpus
                :ignoring

                ;; hash-tables
                :dict
                :do-hash-table ;; trivial-do
                :dict*
                :dictq  ;; quoted
                :href  ;; nested lookup. Also @.
                :href-default
                :pophash
                :swaphash
                :hash-fold
                :maphash-return
                :merge-tables
                :flip-hash-table
                :set-hash-table
                :hash-table-set
                :hash-table-predicate
                :hash-table-function
                :make-hash-table-function
                :delete-from-hash-table
                :pairhash
                ;; to be continued
                ))


(defpackage ciel-user
  (:use :ciel))

(defpackage generic-ciel
  (:use :generic-cl
        :ciel))

(in-package :ciel)

;; Enable triple quotes for the functions docstring.
;; (in-readtable pythonic-string-reader:pythonic-string-syntax)
(pythonic-string-reader:enable-pythonic-string-syntax)
