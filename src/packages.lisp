(in-package :cl-user)

(uiop:define-package ciel
    (:use :cl)
  ;; xxx: nicknames copied from ciel-user below.
  (:local-nicknames (:/os :uiop/os)
                    (:os :uiop/os)
                    (:filesystem :uiop/filesystem)
                    (:finder :file-finder)
                    (:notify :org.shirakumo.file-notify)
                    (:alex :alexandria)
                    (:csv :cl-csv)
                    (:http :dexador)
                    (:json :shasht)
                    (:json-pointer :cl-json-pointer/synonyms)
                    (:time :local-time)
                    (:routes :easy-routes))
  (:export #:-->
           #:^
           #:load-without-shebang))

(uiop:define-package ciel-user
  (:use :cl :ciel)
  (:local-nicknames (:/os :uiop/os)     ; let's try this nickname.
                    ;; Simply :os doesn't help at auto-discovery with SLIME's autocompletion.
                    ;; But let's add it anyways for correctness,
                    ;; it's handy for the shell and scripts.
                    (:os :uiop/os)
                    ;; This other uiop module is always useful:
                    (:filesystem :uiop/filesystem)
                    (:finder :file-finder)
                    (:notify :org.shirakumo.file-notify)

                    (:alex :alexandria)
                    (:csv :cl-csv)
                    (:http :dexador)
                    (:json :shasht)
                    (:json-pointer :cl-json-pointer/synonyms)
                    (:time :local-time)
                    (:routes :easy-routes))
  (:export
   #:*script-args*))

(uiop:define-package ciel-5am-user
    (:use :cl :ciel
          ;; one addition from ciel-user:
          :5am)
  (:local-nicknames (:/os :uiop/os)     ; let's try this nickname.
                    ;; Simply :os doesn't help at auto-discovery with SLIME's autocompletion.
                    ;; But let's add it anyways for correctness,
                    ;; it's handy for the shell and scripts.
                    (:os :uiop/os)
                    ;; This other uiop module is always useful:
                    (:filesystem :uiop/filesystem)
                    (:finder :file-finder)
                    (:notify :org.shirakumo.file-notify)

                    (:alex :alexandria)
                    (:csv :cl-csv)
                    (:http :dexador)
                    (:json :shasht)
                    (:json-pointer :cl-json-pointer/synonyms)
                    (:time :local-time)
                    (:routes :easy-routes))
  (:documentation "Same package as ciel-user, with the added symbols of fiveam, in order to define ad run unit tests."))


;TODO: a conflict between Serapeum and generic-cl
(uiop:define-package generic-ciel
  (:use :generic-cl
        :ciel)
  ;XXX: local nicknames are duplicated in each package declaration.
  (:local-nicknames (:csv :cl-csv)
                    (:http :dexador)))
