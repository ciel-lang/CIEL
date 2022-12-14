;;;
;;; Run with:
;;; $ ciel simpleHTTPserver.lisp 4242
;;;
;;; or add a shebang line and make this script executable.
;;;

(in-package :ciel-user)

;; CLI args: the script name, an optional port number.
(defparameter *port* (or (parse-integer (second (uiop:command-line-arguments)))
                         8000))

(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor
                     :document-root "./"
                     :port *port*))
(hunchentoot:start *acceptor*)

;; Serve static assets under a static/ directory (optional).
(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/"  "static/"  ;; starts without a /
       )
      hunchentoot:*dispatch-table*)

(format! t "~&Serving files on port ~a…~&" *port*)
(handler-case
    (sleep most-positive-fixnum)
  (sb-sys:interactive-interrupt ()
    (format! t "Bye!")))
