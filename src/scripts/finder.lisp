#!/usr/bin/env ciel

;;
;; $ ciel finder.lisp foo bar | xargs mpv
;;
;; Searches for files matching "foo" and "bar" (in any order) in my music directories.
;;
;; o/
;;

(in-package :ciel-user)

(defparameter *directories* '("~/Music/" "~/Downloads/"))

(defun find-on-directory (root params)
  (finder:finder*
   :root root
   ;; "and" the params: needs ongoing PR.
   ;; :predicates (apply #'finder/p:every-path~ params)))
   ;; does a "or":
   :predicates (apply #'finder:path~ params)))

(defun find-files (&optional params)
  (unless params
    (format *error-output* "No search terms supplied.~&Usage: finder.lisp search terms.~&")
    (return-from find-files))
  (let ((str:*ignore-case* t)
        (params (ensure-list params)))
    (flatten
     (loop for root in *directories*
           collect
           (find-on-directory root params)))))

(defun pprint-for-shell (list)
  (mapcar (lambda (p)
            (format t "~s~&" (finder:path p)))
          list)
  (terpri))

#+ciel
(pprint-for-shell (find-files (rest *script-args*)))
