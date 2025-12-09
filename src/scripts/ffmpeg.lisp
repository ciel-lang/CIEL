(in-package :ciel-user)

;;;
;;; Search files and transform them to mp3 with ffmpeg.
;;;
;;; Usage:
;;;
;;; ciel src/scripts/ffmpeg search terms
;;;
;;; Todo:
;;; - choose search directories (do we even search on the current directory?)
;;; - choose output format, etc.
;;;
;;; TIL: we need to escape file names if they contain characters such as [ ].
;;;

(defparameter *directories* '("~/Music/" "~/Downloads/" "~/zique/"))

(defvar *music-type-extensions* '("aac" "ac3" "aiff" "amr" "ape" "dts" "f4a" "f4b" "flac" "gsm"
         "m3u" "m4a" "midi" "mlp" "mka" "mp2" "mp3" "oga" "ogg" "opus" "pva"
                                  "ra" "ram" "raw" "rf64" "spx" "tta" "wav" "wavpack" "wma" "wv")
  "A bunch of audio extensions. Should be supported by ffmpeg. Thanks ready-player.el.")

(defun music-file-p (file)
  "Return non-NIL if this file (pathname) has a music file extension as of *music-type-extensions*."
  (find (pathname-type file) *music-type-extensions* :test #'equalp))


(defun find-on-directory (root params)
  "Search on default directories.

  PARAMS (list)"
  (finder:finder*
   :root root
   ;; This "and"s the params:
   :predicates (apply #'finder:every-path~ params)
   ;; This would do a "or":
   ;; :predicates (apply #'finder:path~ params)
   ))

(defun find-files (&optional params)
  "Find files matching PARAMS (a list of strings) on the default directories.

  PARAMS is an 'OR'. I would prefer to 'and' the matches actually (see find-on-directory)."
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
  "Pretty-print this list of files (with full path), one per line."
  (mapcar (lambda (p)
            (format t "~s~&" (finder:path p)))
          list)
  (terpri))

(defun change-extension (file)
  (when (finder:file? file)
    (setf file (finder:path file)))
  (let ((extension (pathname-type file)))
    (when extension
      (values
       (str:replace-all extension "mp3" file)
       extension))))

;; warn!
(defun escape-file-name (name)
  "Escape [ and ] with double \\,

  Or test the file exists with:

  (probe-file (make-pathname :name name :type extension))

  this doesn't choke with wildcard characters such as [ and ].

  otherwise uiop:file-exists-p returns NIL for an existing file."
  ;; This works on upstream file-finder <2025-09-09>
  ;; (when (finder:file? name)
    ;; (setf name (finder:path name)))
  (str:replace-using '("[" "\\["
                       "]" "\\]")
                     name))

(defun run-ffmpeg (file)
  "Run ffmpeg on FILE, transform to mp3."
  (let ((target (change-extension file)))
    (if (uiop:file-exists-p (escape-file-name target))
        (progn
          (format t "~&mp3 already exists: ~a~&" target)
          target)
        (uiop:run-program (list "ffmpeg"
                                "-i"
                                (finder:path file)
                                target)
                          :output :interactive
                          :error-output t))))

(defun ffmpeg-on-files (files)
  "Transform files to mp3 with ffmpeg.

  Search on the default directories by AND-ing the search terms."
  (loop for file in files
        for path = (finder:path file)
        when (and (music-file-p path)
                  (uiop:file-exists-p (escape-file-name path)))
          do (format t "~&transforming: ~a~&" file)
             (run-ffmpeg file)
          and collect file into processed
        finally
           (format t "~&~%done for files: ~&")
           (pprint-for-shell processed)))

#+ciel
(ffmpeg-on-files (find-files (rest *script-args*)))
