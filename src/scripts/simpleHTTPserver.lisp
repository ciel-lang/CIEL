#!/usr/bin/env ciel
;;;
;;; Run with:
;;; $ ciel -s simpleHTTPserver 4242
;;;
;;; or
;;;
;;; $ ./simpleHTTPserver.lisp
;;;
;;; Use -b to open the web browser.

(in-package :ciel-user)

(use-package :spinneret)

;; ;;;;;;;;;;;;;;;;;;;;;;,
;; CLI args.
;; ;;;;;;;;;;;;;;;;;;;;;;,
(defparameter *my-cli-args* *script-args*
  "Copy of the CLI options given to this script.
  When we call

$ ciel -v simpleHTTPserver.lisp -b 4444

then the list '(simpleHTTPserver.lisp -b 4444) is given in *script-args*.")

(defparameter *cli/browse-p*
  (when (member "-b" *script-args* :test #'equal)
    (setf *my-cli-args* (remove "-b" *script-args* :test #'equal))
    t)
  "Option -b: open the localhost URL with our browser (using xdg-open).")

;; CLI args: the script name, an optional port number.
(defparameter *port* (or (ignore-errors (parse-integer (second *my-cli-args*)))
                         9000))

(defparameter *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                        :document-root "./"
                                        :port *port*))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Templates" (with s-expressions and Spinneret)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (unless (fboundp 'str:ensure-suffix)
    (print "WARN: str:ensure-suffix is in the latest cl-str. You'll need to clone it into ~/quicklisp/local-projects/"))
  (if (uiop:file-pathname-p path)
      (file-namestring path)
      ;; How to simply get the directory name, not full path?
      ;; pathname-directory -> (:relative "path" "to" "dir")
      ;; INFO: this is merged in cl-str but not in Quicklisp yet.
      (str:ensure-suffix "/"
                         (first (last (pathname-directory path))))))

(defun show-file-list (file-list &key title)
  "List files in a HTML list."
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

;; ;;;;;;;;;;;;;;;;;;;;;;
;; web-server config.
;;;;;;;;;;;;;;;;;;;;;;;;,

;; On the root URL "/" show the listing, but when clicking on a file let the server serve it.
(defun serve-root ()
  (push
   (hunchentoot:create-regex-dispatcher "/$" #'file-list)
   hunchentoot:*dispatch-table*))

#+(or)
(setf hunchentoot:*dispatch-table* nil)

(defun serve-static-assets ()
  ;; Serve static assets under a static/ directory (optional).
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/"  "static/" ;; starts without a /
         )
        hunchentoot:*dispatch-table*))

;; main function:
(defun simplehttpserver ()
  "Create a Hunchentoot dispatcher on our root directory,
  serve static assets,
  start Hunchentoot and keep it on the foreground."
  (serve-root)
  (serve-static-assets)
  (handler-case
      (progn
        ;; Start the webserver.
        (hunchentoot:start *acceptor*)
        (format! t "~&Serving files on port ~a…~&" *port*)
        (format! t "~&~&~t ⤷ http://127.0.0.1:~a ~&~&" *port*)

        #+unix
        (when (member "-b" uiop:*command-line-arguments* :test #'equal)
          (uiop:format! t "Open web browser…~&")
          (uiop:run-program (list
                             "xdg-open"
                             (format nil "http://localhost:~a" *port*))))

        ;; Wait in the foreground.
        (sleep most-positive-fixnum))

    (usocket:address-in-use-error ()
      (format! *error-output* "This port is already in use. Quitting.~&"))
    (sb-sys:interactive-interrupt ()
      (format! t "Bye!")
      (hunchentoot:stop *acceptor*))
    (error ()
      (format! t "An error occured. Quitting.")
      (hunchentoot:stop *acceptor*))))

;; Call the main function only when running this as a script,
;; not when developing on the REPL.
#+ciel
(simplehttpserver)
