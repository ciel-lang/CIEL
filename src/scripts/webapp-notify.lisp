#!/usr/bin/env ciel
;;;
;;; Run with:
;;; $ ./webapp-notify.lisp
;;;
;;; Watch this file for write events and load & compile it again.
;;; This redefines our web routes, so we can develop our app in a simple interactive way.
;;;
;;; If you are doing that, you'll want to setup a proper dev environment to enjoy full Common Lisp image-based development.

(in-package :ciel-user)

(routes:defroute route-root "/" (&get name)
  (format nil "Hello ~a!" (or name (os:getenv "USER") "lisper")))

(routes:defroute route-hello "/hello" ()
  (format nil "Hello :)"))

;; Try adding new routes.
;; (gotcha: give them unique names)

(defvar *server* nil)

(defun start-webapp ()
  (unless *server*
    (setf *server* (make-instance 'routes:easy-routes-acceptor :port 4567))
    (hunchentoot:start *server*)))

(defun stop-webapp ()
  (hunchentoot:stop *server*))

(defun dumb-auto-reload ()
  "Watch this file for write events and load & compile it again.
  This redefines our web routes, so we can develop our app in a simple interactive way.

  If you are doing that, you'll want to setup a proper dev environment to enjoy full Common Lisp image-based development."
  (format! t "~&Watching webapp.lisp…")
  (notify:watch "webapp.lisp")
  (notify:with-events (file change :timeout T)
    ;; list of events:
    ;; (print (list file change))
    (when (equal change :close-write)
      (format! t "~%~%Reloading ~a…~&" file)
      (handler-case
          (ciel::load-without-shebang "webapp.lisp")
        (reader-error ()
          ;; READ errors, parenthesis not closed, etc. Wait for the developer.
          (format! t "~%~%read error, waiting for change…~&"))))))


#+ciel
(unless *server*
  (start-webapp)
  (format t "~&App started on localhost:4567…~&")
  (dumb-auto-reload)
  (sleep most-positive-fixnum))
