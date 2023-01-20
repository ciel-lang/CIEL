;;;
;;; Run with:
;;; $ ciel simpleHTTPserver.lisp 4242
;;;
;;; or add a shebang line and make this script executable.
;;;

(in-package :ciel-user)

;; CLI args: the script name, an optional port number.
(defparameter *port* (or (ignore-errors (parse-integer (second uiop:*command-line-arguments*)))
                         9000))

(defparameter *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                        :document-root "./"
                                        :port *port*))
(hunchentoot:start *acceptor*)

;; Serve static assets under a static/ directory (optional).
(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/"  "static/"  ;; starts without a /
       )
      hunchentoot:*dispatch-table*)

(handler-case
    (progn
      ;; Start the webserver.
      (hunchentoot:start *acceptor*)
      (format! t "~&Serving files on port ~a…~&" *port*)
      (format! t "~&~&~t ⤷ http://127.0.0.1:~a ~&~&" *port*)

      ;; Wait in the foreground.
      (sleep most-positive-fixnum))

  (usocket:address-in-use-error ()
    (format! *error-output* "This port is already in use. Quitting.~&"))
  (sb-sys:interactive-interrupt ()
    (format! t "Bye!")
    (hunchentoot:stop *acceptor*))
  (error ()
    (format! t "An error occured. Quitting.")
    (hunchentoot:stop *acceptor*)))
