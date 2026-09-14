#!/usr/bin/env ciel
;;;
;;; description: example web app, defining a route on /
;;; with an optional "name" URL parameter.
;;; The app is reachable through your server's IP.
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

#+ciel
(progn
  (start-webapp)
  (format t "~&App started on localhost:4567…~&")
  (format t "It accepts an optional URL parameter: localhost:4567/?name=you")
  (sleep most-positive-fixnum))
