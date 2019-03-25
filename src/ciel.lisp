(in-package :cl-user)
(defpackage ciel
  (:use :cl
        :alexandria
        :arrow-macros
        :cl-csv
        :cl-ppcre
        :defclass-std
        :fn
        :for
        :local-time
        :modf
        :parse-float
        :cl-ansi-text
        :trivia))

;; conflicts:
;; iterate and for
;; sxql::returning and for::returning
;; dissect:object and for-iterator:object


(defpackage ciel-user
  (:use :cl
        :ciel))

(defpackage generic-ciel
  (:use :generic-cl
        :ciel))

(in-package :ciel)
;; enable ^(* 2 _) syntax.
(cl-punch:enable-punch-syntax)
