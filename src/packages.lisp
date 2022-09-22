(in-package :cl-user)

(uiop:define-package ciel
  (:use :cl)
  (:export #:enable-shell-passthrough
           #:-->)
  (:local-nicknames (:/os :uiop/os)
                    ;; let's try this nickname.
                    ;; Simply :os doesn't help at auto-discovery with SLIME's autocompletion.
                    ;; but let's add it anyways for correctness.
                    (:os :uiop/os)
                    (:alex :alexandria)
                    (:csv :cl-csv)
                    (:http :dexador)))

(uiop:define-package ciel-user
  (:use :cl :ciel)
  (:local-nicknames (:csv :cl-csv)
                    (:http :dexador)
                    (:json :shasht)))

;TODO: a conflict between Serapeum and generic-cl
(uiop:define-package generic-ciel
  (:use :generic-cl
        :ciel)
  ;XXX: local nicknames are duplicated in each package declaration.
  (:local-nicknames (:csv :cl-csv)
                    (:http :dexador)))
