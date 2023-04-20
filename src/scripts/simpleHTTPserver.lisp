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

;; CLI args: the script name, an optional port number.
(defparameter *port* 9000
  "Default port. Change it with a free argument on the command-line.")

(defparameter *acceptor* nil
  "Hunchentoot's server instance. Create it with `make-acceptor'.")

(defun make-acceptor (&key (port *port*))
  "Return the existing acceptor or create one."
  ;; I prefer to put this in a function to develop interactively.
  (or *acceptor*
      (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                      :document-root "./"
                                      :port port))))

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
(defun simplehttpserver (&key browse (port *port*))
  "Create a Hunchentoot dispatcher on our root directory,
  serve static assets,
  start Hunchentoot and keep it on the foreground."
  (serve-root)
  (serve-static-assets)
  (handler-case
      (progn
        ;; Start the webserver.
        (hunchentoot:start (make-acceptor :port port))
        (format! t "~&Serving files on port ~a…~&" port)
        (format! t "~&~&~t ⤷ http://127.0.0.1:~a ~&~&" port)

        #+unix
        (when browse
          (uiop:format! t "Open web browser…~&")
          (uiop:run-program (list
                             "xdg-open"
                             (format nil "http://localhost:~a" port))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse CLI arguments with Clingon.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter cli/options
  (list
   (clingon:make-option
    :flag
    :description "Show help"
    :short-name #\h
    :key :help)
   (clingon:make-option
    :flag
    :description "Open browser"
    :short-name #\b
    :long-name "browse"
    :key :browse))
  "Our script's options.")

(defun cli/handler (cmd)
  "Look at our CLI args and eventually start the web server.

  cmd: a Clingon command built with cli/command."
  (let* ((help (clingon:getopt cmd :help))
         (browse (clingon:getopt cmd :browse))
         ;; freeargs always have the script name??
         (freeargs (rest (clingon:command-arguments cmd)))
         (port *port*))
    (when help
      ;; This funcall is to avoid a style warning: the cli/command function
      ;; is not yet defined.
      (clingon:print-usage (funcall 'cli/command) t)
      (return-from cli/handler))
    (when freeargs
      (setf port (or (ignore-errors
                      (parse-integer (first freeargs)))
                     *port*)))
    (simplehttpserver :browse browse :port port)
    ))

(defun cli/command ()
  "Create a top-level Clingon command."
  (clingon:make-command
   :name "simpleHTTPserver.lisp"
   :description "Serve the local directory with a simple web server."
   :usage "[-h] [-b] [PORT]"
   :version "0.1"
   :license "todo"
   :authors '("vindarel")
   :options cli/options
   :handler #'cli/handler))

#|
A note on using Clingon's free arguments in scripts.
(we get them with (clingon:command-arguments))

$ ciel -s simplehttpserver 4242
only has 4242 as free argument, but

$ ./simpleHTTPserver.lisp 4242
under the hood equals to
$ ciel simpleHTTPserver.lisp 4242
and this has two free arguments: simplehttpserver.lisp and 4242.

To fix this, to make it always coherent so we can run this script with
-s <name> or with the shebang, CIEL sets *script-args* to always have
the script name.

|#

;; Call the main function only when running this as a script,
;; not when developing on the REPL.
#+ciel
(clingon:run (cli/command) *script-args*)
