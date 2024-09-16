#!/usr/bin/env ciel
;;;
;;; Run with:
;;; $ ./webapp.lisp
;;;

(in-package :ciel-user)

(routes:defroute route-root "/" (&get name)
  (format nil "Hello ~a!" (or name (os:getenv "USER") "lisper")))

(defvar *server* nil)

(defun start-webapp ()
  (setf *server* (make-instance 'routes:easy-routes-acceptor :port 4567))
  (hunchentoot:start *server*))

(defun stop-webapp ()
  (hunchentoot:stop *server*))

#+ciel-script
(progn
  (start-webapp)
  (format t "~&App started on localhost:4567…~&")
  (sleep most-positive-fixnum))
