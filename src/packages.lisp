(in-package :cl-user)

(uiop:define-package ciel
  (:use :cl)
  (:export #:enable-shell-passthrough
           #:disable-shell-passthrough
           #:-->))

(uiop:define-package ciel-user
  (:use :cl :ciel)
  (:local-nicknames (:/os :uiop/os)     ; let's try this nickname.
                    ;; Simply :os doesn't help at auto-discovery with SLIME's autocompletion.
                    ;; But let's add it anyways for correctness,
                    ;; it's handy for the shell and scripts.
                    (:os :uiop/os)
                    ;; This other uiop module is always useful:
                    (:filesystem :uiop/filesystem)

                    (:alex :alexandria)
                    (:csv :cl-csv)
                    (:http :dexador)
                    (:json :shasht)

                    (:routes :easy-routes)))

;TODO: a conflict between Serapeum and generic-cl
(uiop:define-package generic-ciel
  (:use :generic-cl
        :ciel)
  ;XXX: local nicknames are duplicated in each package declaration.
  (:local-nicknames (:csv :cl-csv)
                    (:http :dexador)))
