;;;
;;; Extra config to build the binary,
;;; stuff that doesn't need to be in the .asd
;;; (and would be harmful to be there).
;;;

;; We want to configure cl+ssl for the Deploy binary,
;; and we need to load cl+ssl before we can load this config.
;; If it was in the .asd, we would have to load cl+ssl before being able
;; to load the .asd file, which is annoying
;; (it impeds loading CIEL in the REPL with a usual load-asd (C-c C-k)
;; and complicates Ultralisp or Quicklisp distribution).
;;
;; So, we need cl+ssl to build the binary with asdf:make
;; See also the Makefile that quickloads cl+ssl already (maybe this below isn't
;; required any more?)
(unless (find-package :cl+ssl)
  (warn "Loading build-config.lisp: we don't find the package CL+SSL. You need to install it before loading this config file and building CIEL's binary. Let's install it with Quicklisp now.~&")
  (ql:quickload "cl+ssl"))
(require "cl+ssl")

;; Don't ship libssl, rely on the target OS'.
#+linux (deploy:define-library cl+ssl::libssl :dont-deploy T)
#+linux (deploy:define-library cl+ssl::libcrypto :dont-deploy T)

;; Use compression: from 114M, 0.02s startup time to 27M and 0.42s (SBCL 2.0.10).
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

;; Even with the binary, ASDF wants to update itself and crashes
;; if it doesn't find an ASDF directory, like on a user's system.
;; Thanks again to Shinmera.
(deploy:define-hook (:deploy asdf) (directory)
  (declare (ignorable directory))
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () nil))
