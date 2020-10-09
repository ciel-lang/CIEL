(in-package :cl-user)
(defpackage ciel
  (:use :cl))

(in-package :ciel)

(cl-reexport:reexport-from :parse-float)

(cl-reexport:reexport-from :access
                           :include '(:access
                                      :accesses))
;XXX: it's only in the latest Quicklisp
;; (cl-reexport:reexport-from :trivial-do
;;                            :include
;;                            '(:doalist ;; key value alist
;;                              :dohash  ;; key value ht
;;                              :dolist* ;; position value list
;;                              :doplist ;; key value plist
;;                              :doseq   ;; value sequence
;;                              :doseq*  ;; position value sequence
;;                              ))

(cl-reexport:reexport-from :repl-utilities
                           :include
                           '(:repl-utilities
                             :readme
                             :doc
                             :summary
                             :package-apropos
                             :trace-package
                             :print-hash))

(cl-reexport:reexport-from :serapeum
                           :include
                           '(:assort
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
                             :do-hash-table ;; see also trivial-do
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
  (:use :cl :ciel)
  (:local-nicknames (:match :trivia)))

;TODO: a conflict between Serapeum and generic-cl
;; (defpackage generic-ciel
  ;; (:use :generic-cl
        ;; :ciel))

(in-package :ciel-user)

;; Enable triple quotes for the functions docstring.
;; (in-readtable pythonic-string-reader:pythonic-string-syntax)
(pythonic-string-reader:enable-pythonic-string-syntax)
