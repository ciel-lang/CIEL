(in-package :cl-user)
(defpackage ciel
  (:use :cl
        :alexandria
        :arrow-macros
        :cl-csv
        :cl-ppcre
        :fn
        ;; :for
        ;; :iterate ;; conflicts with for
        :local-time
        :modf
        :parse-float
        :cl-ansi-text
        :trivia))

;; conflicts:
;; for::returning and sxql::returning
;; for::  and iterate

(in-package :ciel)
;; enable ^(* 2 _) syntax.
(cl-punch:enable-punch-syntax)
