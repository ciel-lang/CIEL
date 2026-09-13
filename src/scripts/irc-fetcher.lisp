(in-package :ciel-user)

(use-package '(local-time
               ;; periods
               ))
(import 'periods::do-times)

;;;
;;; First use:
;;; ciel irc-fetcher.lisp 2026-09-10
;;; to fetch the #commonlisp IRC channel from this date.
;;;
;;; The script saves today's date to ~/.irc-log-reader-last
;;; and next calls use it to resume fetching.
;;;
;;; This script uses local-time's parse-timestring, format-timestring
;;; and period's do-times, duration
;;;
;;; src: https://gist.github.com/bo-tato/9df8182150dc49d0943f13635430b647
;;;


(defconstant +last-date-file+ "~/.irc-log-reader-last")

(defun last-date (&aux (cli-arg (second ciel-user:*script-args*)))
  "Read the last date to parse IRC: from a CLI arg or the ~/.irc-log-reader-last file."
  (cond
    ((str:non-blank-string-p cli-arg)
     (-> cli-arg
         parse-timestring))
    ((uiop:file-exists-p +last-date-file+)
     (-> +last-date-file+
         str:from-file
         str:trim
         parse-timestring))
    (t
     (format t "Please give the day to start fetching IRC as argument or in ~a. Next calls use this file.~&" +last-date-file+)
     (termp:quit))))


(defun fmt-date (date)
  "Format as YYYY-MM-DD"
  (format-timestring nil date :format +rfc3339-format/date-only+))

(defun fetchit (date)
  (do-times (day
             date
             (periods:duration :days 1)
             (today))
    (format! t "Fetching day ~a...~&" day)
    (let* ((url (str:concat "https://libera.irclog.whitequark.org/commonlisp/" (fmt-date day)))
           (doc (lquery:$ (initialize (dex:get url)))))
      (format t "~%:: ~a ::~%" day)
      (lquery:$ doc ".talk.op-msg" (text)
        ;; (map [#'println {substitute #\Space #\Newline}]))))
        (map #'println)))))

;; Run it:
#+ciel
(progn
  (fetchit (last-date))
  (str:to-file +last-date-file+ (fmt-date (today))))
