;;; description: install Quicklisp with HTTPS via curl.

(in-package :ciel-user)

#|

error:

Cloning into '/home/debian/common-lisp/ql-https'...
remote: Enumerating objects: 327, done.
remote: Counting objects: 100% (98/98), done.
remote: Compressing objects: 100% (30/30), done.
remote: Total 327 (delta 77), reused 69 (delta 68), pack-reused 229 (from 2)
Receiving objects: 100% (327/327), 78.30 KiB | 2.53 MiB/s, done.
Resolving deltas: 100% (155/155), done.
Running setup code...
This is SBCL 2.5.2.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* ("ASDF" "asdf" "UIOP" "uiop")
* T
* ; compiling file "/home/debian/common-lisp/ql-https/ql-https.lisp" (written 14 SEP 2026 11:44:39 AM):

debugger invoked on a PACKAGE-DOES-NOT-EXIST in thread
#<THREAD tid=649326 "main thread" RUNNING {1003F680A3}>:
  The name "QL-GUNZIPPER" does not designate any package.


(it's in quicklisp/packages.lisp)

|#

(defparameter *setup-for-init-file* ";;; The following lines were added when you installed Quicklisp with ciel -s install-quicklisp.
;;; This loads Quicklisp when you start CIEL, so you can use Quicklip straight away.
#-quicklisp
(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
")

(defun install-ql-with-https ()
  "Call out to cURL to install Quicklisp."
  ;; Also call with bash.
  ;; The script is stored in a string… we put it back to a file.
  (uiop:with-temporary-file (:pathname f)
    (format! t "Calling Quicklisp installer…~&")
    #-unix
    (format! *error-output* "we need to make the script file executable.~&")
    #+unix
    (uiop:run-program (list "chmod" "+x" (uiop:native-namestring f)))
    (str:to-file f ciel::*ql-https-install.sh*)
    (uiop:run-program (list f)
                      :output t
                      :error-output t))
  (format! t "done.~&"))

(defun add-to-init-file (file &key (snippet *setup-for-init-file*))
  (with-open-file (stream (uiop:native-namestring file)
                        :direction :output
                        :if-exists :append)
    (format stream "~%~a~%" snippet)))

(defun add-to-cielrc ()
  ;; (load (make-string-input-stream ciel::*quicklisp.lisp*))
  (handler-case
      (add-to-init-file "~/.cielrc")
    (error (c)
      (format *error-output* "~&Error while adding Quicklisp setup to ~~/.cielrc: ~a~&" c))))


#+(and ciel unix)
(progn

  ;; Install.
  (handler-case
      (progn
        ;; and run-program would print the whole script content to say it exited with an error code.
        (install-ql-with-https)

        ;; Configure.
        (add-to-cielrc))
  (error (c)
         (format! *error-output* "~a" c))))

#+(and ciel windows)
(error "We currently use a shell script to install ql-https. Feel free to open an issue.")
