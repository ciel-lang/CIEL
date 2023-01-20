;;;
;;; Run with:
;;; $ ciel -s simpleHTTPserver 4242
;;;
;;; or add a shebang line and make this script executable.
;;;

(in-package :ciel-user)

(use-package :spinneret)

;; CLI args: the script name, an optional port number.
(defparameter *port* (or (ignore-errors (parse-integer (second uiop:*command-line-arguments*)))
                         9000))

(defparameter *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                        :document-root "./"
                                        :port *port*))

(defmacro with-page ((&key title) &body body)
  `(with-html-string
       (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))

(defun file-or-directory-namestring (path)
  "Return the name of this file or of this directory.
  XXX: we want the name of the directory, not the full path."
  (if (uiop:file-pathname-p path)
      (file-namestring path)
      ;; How to simply get the directory name, not full path?
      ;; pathname-directory -> (:relative "path" "to" "dir")
      (str:ensure-ends-with "/"
                            (first (last (pathname-directory path))))))

(defun show-file-list (file-list &key title)
  (with-page (:title title)
    (:header
     (:h2 title))
    (:ol (dolist (item file-list)
           (:li (:a :href
                    (format nil "~a" (file-or-directory-namestring item))
                    (format nil "~a" (file-or-directory-namestring item))))))
    (:br)
    (:footer :style "color: dimgrey" ("Powered by CIEL Is an Extended Lisp" ))))

(defun file-list ()
  (show-file-list (append
                   ;; This is how to list directories,
                   ;; but we have to serve their content now.
                   ;; (uiop:subdirectories (uiop:getcwd))
                   (uiop:directory-files (uiop:getcwd)))
                  :title (format nil "Files for ~a" (uiop:getcwd))))

;; On the root URL "/" show the listing, but when clicking on a file let the server serve it.
(defun serve-root ()
  (push
   (hunchentoot:create-regex-dispatcher "/$" #'file-list)
   hunchentoot:*dispatch-table*))

(serve-root)

#+(or)
(setf hunchentoot:*dispatch-table* nil)

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
